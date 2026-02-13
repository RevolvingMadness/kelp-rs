use std::collections::BTreeMap;

use crate::compile_context::CompileContext;
use crate::data_type::{DataTypeKind, high::HighDataType};
use crate::datapack::HighDatapack;
use crate::expression::{Expression, constant::ConstantExpressionKind};
use crate::pattern::Pattern;
use crate::semantic_analysis_context::{
    Scope, SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
    SemanticAnalysisInfoKind,
};
use crate::trait_ext::OptionIterExt;
use minecraft_command_types::command::Command;
use minecraft_command_types::command::data::{
    DataCommand, DataCommandModification, DataCommandModificationMode,
};
use minecraft_command_types::command::execute::{ExecuteIfSubcommand, ExecuteSubcommand};
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::{SNBT, SNBTString};
use nonempty::nonempty;
use parser_rs::parser_range::ParserRange;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    MCFunction(ResourceLocation, Box<Statement>),
    Expression(Expression),
    VariableDeclaration(Option<HighDataType>, Pattern, Expression),
    While(Expression, Box<Statement>),
    Match(Expression, BTreeMap<IntegerRange, Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    ForIn(bool, String, Expression, Box<Statement>),
    Block(Vec<Statement>),
    AppendData(Expression, Box<Expression>),
    RemoveData(Expression),
}

fn compile_if(
    datapack: &mut HighDatapack,
    caller_ctx: &mut CompileContext,
    mut body_ctx: CompileContext,
    inverted: bool,
    condition: ExecuteIfSubcommand,
) {
    let should_inine = body_ctx.num_commands() <= 5;

    if should_inine {
        for command in body_ctx.commands {
            caller_ctx.add_command(
                datapack,
                Command::Execute(
                    ExecuteSubcommand::If(inverted, condition.clone())
                        .then(ExecuteSubcommand::Run(Box::new(command))),
                ),
            );
        }
    } else {
        let body_paths = datapack.get_unique_function_paths();

        caller_ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::If(inverted, condition).then(
                ExecuteSubcommand::Run(Box::new(Command::Function(
                    ResourceLocation::new_namespace_paths(
                        datapack.current_namespace_name(),
                        body_paths.clone(),
                    ),
                    None,
                ))),
            )),
        );

        datapack.compile_and_add_to_function(&body_paths, &mut body_ctx);
    }
}

impl StatementKind {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            StatementKind::MCFunction(id, statement) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    statement.kind.compile(datapack, &mut function_ctx);

                    let function_commands = function_ctx.compile();

                    datapack
                        .current_namespace_mut()
                        .current_function_mut()
                        .add_commands(function_commands);

                    datapack.pop_function_from_current_namespace();
                });
            }
            StatementKind::Expression(expression) => {
                expression
                    .resolve(datapack, ctx)
                    .compile_as_statement(datapack, ctx);
            }
            StatementKind::VariableDeclaration(data_type, pattern, value) => {
                let value = value.resolve_force(datapack, ctx);

                pattern.kind.destructure(
                    datapack,
                    data_type,
                    value.into_dummy_constant_expression(),
                );
            }
            StatementKind::While(condition, body) => {
                let mut while_body_ctx = CompileContext::default();

                body.kind.compile(datapack, &mut while_body_ctx);

                let mut condition_ctx = CompileContext::default();
                let (should_be_inverted, condition) = condition
                    .resolve(datapack, &mut condition_ctx)
                    .to_execute_condition(datapack, &mut condition_ctx, false);

                let while_function_paths = datapack.get_unique_function_paths();

                let current_namespace_name = datapack.current_namespace_name().to_string();

                condition_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(should_be_inverted, condition).then(
                        ExecuteSubcommand::Run(Box::new(Command::Function(
                            ResourceLocation::new_namespace_paths(
                                current_namespace_name,
                                while_function_paths.clone(),
                            ),
                            None,
                        ))),
                    )),
                );

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);
            }
            StatementKind::ForIn(is_reversed, variable_name, collection, body) => {
                let collection_data_type = collection
                    .kind
                    .infer_data_type(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap_or_else(|| panic!("Expression {:?} is not iterable", collection));

                let collection = collection.resolve(datapack, ctx);

                if collection_data_type.equals(&DataTypeKind::String) {
                    let (unique_data_target, unique_path, name) = datapack.get_unique_data_named();
                    let (unique_data_target_2, unique_path_2) = datapack.get_unique_data();

                    collection.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    datapack.start_scope();
                    datapack.declare_variable(
                        &variable_name,
                        DataTypeKind::Data(Box::new(DataTypeKind::SNBT)),
                        ConstantExpressionKind::Data(
                            unique_data_target_2.clone(),
                            unique_path_2.clone(),
                        )
                        .into_dummy_constant_expression(),
                    );
                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            unique_data_target_2,
                            unique_path_2,
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.clone(),
                                Some(unique_path.clone()),
                                Some(if is_reversed { -1 } else { 0 }),
                                if is_reversed { None } else { Some(1) },
                            ),
                        )),
                    );
                    body.kind.compile(datapack, &mut for_body_ctx);
                    datapack.end_scope();

                    let current_namespace_name = datapack.current_namespace_name().to_string();

                    let for_function_paths = datapack.get_unique_function_paths();

                    let mut condition_ctx = CompileContext::default();

                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            unique_data_target.clone(),
                            unique_path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.clone(),
                                Some(unique_path.clone()),
                                Some(if is_reversed { 0 } else { 1 }),
                                if is_reversed { Some(-1) } else { None },
                            ),
                        )),
                    );

                    let mut map = BTreeMap::new();
                    map.insert(SNBTString(false, name), SNBT::string(""));
                    let unique_path = NbtPath(nonempty![NbtPathNode::RootCompound(map)]);

                    condition_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            true,
                            ExecuteIfSubcommand::Data(
                                unique_data_target,
                                unique_path,
                                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                    Command::Function(
                                        ResourceLocation::new_namespace_paths(
                                            current_namespace_name,
                                            for_function_paths.clone(),
                                        ),
                                        None,
                                    ),
                                )))),
                            ),
                        )),
                    );

                    for_body_ctx.extend_context(condition_ctx.clone());
                    ctx.extend_context(condition_ctx);

                    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
                } else {
                    let (unique_data_target, unique_path) = datapack.get_unique_data();

                    collection.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let unique_path = unique_path.with_node(NbtPathNode::Index(Some(
                        SNBT::Integer(if is_reversed { -1 } else { 0 }),
                    )));

                    let mut for_body_ctx = CompileContext::default();

                    datapack.start_scope();
                    datapack.declare_variable(
                        &variable_name,
                        DataTypeKind::Data(Box::new(DataTypeKind::SNBT)),
                        ConstantExpressionKind::Data(
                            unique_data_target.clone(),
                            unique_path.clone(),
                        )
                        .into_dummy_constant_expression(),
                    );
                    body.kind.compile(datapack, &mut for_body_ctx);
                    datapack.end_scope();

                    let current_namespace_name = datapack.current_namespace_name().to_string();

                    let for_function_paths = datapack.get_unique_function_paths();

                    let mut condition_ctx = CompileContext::default();

                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Remove(
                            unique_data_target.clone(),
                            unique_path.clone(),
                        )),
                    );

                    condition_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Data(
                                unique_data_target,
                                unique_path,
                                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                    Command::Function(
                                        ResourceLocation::new_namespace_paths(
                                            current_namespace_name,
                                            for_function_paths.clone(),
                                        ),
                                        None,
                                    ),
                                )))),
                            ),
                        )),
                    );

                    for_body_ctx.extend_context(condition_ctx.clone());
                    ctx.extend_context(condition_ctx);

                    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
                }
            }
            StatementKind::Block(body) => {
                datapack.start_scope();

                for statement in body {
                    statement.kind.compile(datapack, ctx);
                }

                datapack.end_scope();
            }
            StatementKind::Match(_, _) => {
                todo!()
            }
            StatementKind::If(condition, body, else_body) => {
                let mut if_body_ctx = CompileContext::default();
                body.kind.compile(datapack, &mut if_body_ctx);

                let (invert, compiled_condition) = condition
                    .clone()
                    .resolve(datapack, ctx)
                    .to_execute_condition(datapack, ctx, false);

                compile_if(
                    datapack,
                    ctx,
                    if_body_ctx,
                    invert,
                    compiled_condition.clone(),
                );

                if let Some(else_body) = else_body {
                    let mut else_body_ctx = ctx.create_child_ctx();
                    else_body.kind.compile(datapack, &mut else_body_ctx);

                    compile_if(datapack, ctx, else_body_ctx, !invert, compiled_condition);
                }
            }
            StatementKind::AppendData(target, source) => {
                let target = target.resolve(datapack, ctx);
                let source = source.resolve(datapack, ctx);

                let (target, path) = target.as_data(datapack, ctx);
                let (source, source_path) = source.as_data(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target,
                        path,
                        DataCommandModificationMode::Append,
                        DataCommandModification::From(source, Some(source_path)),
                    )),
                );
            }
            StatementKind::RemoveData(expression) => {
                let expression = expression.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx);

                ctx.add_command(datapack, Command::Data(DataCommand::Remove(target, path)));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub span: ParserRange,
    pub kind: StatementKind,
}

impl Statement {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            StatementKind::MCFunction(_, statement) => {
                statement.perform_semantic_analysis(ctx, is_lhs)
            }
            StatementKind::Expression(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
            StatementKind::VariableDeclaration(data_type, pattern, value) => {
                let value_error = value.perform_semantic_analysis(ctx, is_lhs).is_none();

                pattern.perform_irrefutablity_semantic_analysis(ctx)?;

                if value_error {
                    pattern.kind.destructure_unknown(ctx);
                    return None;
                }

                let value_type = value.kind.infer_data_type(ctx)?;

                let final_type = if let Some(data_type) = data_type {
                    if data_type.perform_semantic_analysis(ctx).is_none() {
                        pattern.kind.destructure_unknown(ctx);
                        return None;
                    }

                    let data_type = data_type.kind.resolve();

                    if !data_type.equals(&value_type) {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: value.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: data_type.clone(),
                                    actual: value_type,
                                },
                            ),
                        });
                    }
                    data_type
                } else {
                    value_type
                };

                pattern.perform_destructure_semantic_analysis(ctx, value.span, final_type)?;

                Some(())
            }
            StatementKind::While(expression, statement) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);
                let statement_result = statement.perform_semantic_analysis(ctx, is_lhs);

                expression_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;

                if !expression_type.is_condition() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotCondition(expression_type),
                        ),
                    });
                }

                statement_result?;

                Some(())
            }
            StatementKind::Match(_, _) => todo!(),
            StatementKind::If(expression, statement, else_statement) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);
                let statement_result = statement.perform_semantic_analysis(ctx, is_lhs);
                let else_statement_result = else_statement
                    .as_ref()
                    .map(|else_statement| else_statement.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                expression_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;

                if !expression_type.is_condition() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotCondition(expression_type),
                        ),
                    });
                }

                statement_result?;
                else_statement_result?;

                Some(())
            }
            StatementKind::ForIn(_, name, expression, statement) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);
                let statement_result = statement.perform_semantic_analysis(ctx, is_lhs);

                let expression_type = expression.kind.infer_data_type(ctx)?;

                let Some(iterable_type) = expression_type.get_iterable_type() else {
                    ctx.declare_variable_unknown::<()>(name);

                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotIterateType(expression_type),
                        ),
                    });
                };

                ctx.declare_variable_known(name, iterable_type);

                expression_result?;
                statement_result?;

                Some(())
            }
            StatementKind::Block(statements) => {
                ctx.scopes.push_front(Scope::new());

                let result = statements
                    .iter()
                    .map(|statement| statement.perform_semantic_analysis(ctx, is_lhs))
                    .all_some();

                ctx.scopes.pop_front();

                result
            }
            StatementKind::AppendData(target, value) => {
                let target_result = target.perform_semantic_analysis(ctx, is_lhs);
                let value_result = value.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                value_result?;

                Some(())
            }
            StatementKind::RemoveData(target) => target.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    pub fn new(span: ParserRange, kind: StatementKind) -> Statement {
        Statement { span, kind }
    }
}
