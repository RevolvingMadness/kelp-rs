use std::collections::BTreeMap;

use crate::datapack::DataTypeDeclarationKind;
use crate::span::Span;
use crate::trait_ext::OptionUnitIterExt;
use crate::{
    compile_context::CompileContext,
    data_type::{DataTypeKind, high::HighDataType},
    datapack::HighDatapack,
    expression::{Expression, constant::ConstantExpressionKind},
    pattern::Pattern,
    semantic_analysis_context::{
        Scope, SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
};
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
    TypeDeclaration(String, Vec<String>, HighDataType),
    StructDeclaration(String, Vec<String>, BTreeMap<String, HighDataType>),
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
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> Statement {
        Statement { span, kind: self }
    }

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
                let data_type = data_type
                    .map(|data_type| data_type.kind.resolve(datapack, None).unwrap())
                    .unwrap_or(value.kind.infer_data_type(datapack).unwrap());

                let value = value.resolve(datapack, ctx);

                data_type.destructure(
                    datapack,
                    ctx,
                    value.into_dummy_constant_expression(),
                    &pattern,
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
                        variable_name,
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
                            unique_data_target_2.target,
                            unique_path_2,
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.target.clone(),
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
                            unique_data_target.target.clone(),
                            unique_path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.target.clone(),
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
                                unique_data_target.target,
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
                        variable_name,
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
                            unique_data_target.target.clone(),
                            unique_path.clone(),
                        )),
                    );

                    condition_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Data(
                                unique_data_target.target,
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
            StatementKind::AppendData(target, value) => {
                let target = target.resolve(datapack, ctx);
                let value = value.resolve(datapack, ctx);

                let (target, path) = target.as_data(datapack, ctx);

                let modification = value.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.target,
                        path,
                        DataCommandModificationMode::Append,
                        modification,
                    )),
                );
            }
            StatementKind::RemoveData(expression) => {
                let expression = expression.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Remove(target.target, path)),
                );
            }
            StatementKind::TypeDeclaration(name, generics, alias) => {
                let alias = alias.kind.resolve(datapack, Some(&generics)).unwrap();

                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Alias {
                        name,
                        generics,
                        alias,
                    },
                );
            }
            StatementKind::StructDeclaration(name, generics, fields) => {
                let resolved_fields = fields
                    .into_iter()
                    .map(|(key, data_type)| {
                        (
                            key,
                            data_type.kind.resolve(datapack, Some(&generics)).unwrap(),
                        )
                    })
                    .collect();

                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Struct {
                        name,
                        generics,
                        fields: resolved_fields,
                    },
                );
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub span: Span,
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
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }
            StatementKind::VariableDeclaration(explicit_type, pattern, value) => {
                let resolved_explicit_type = explicit_type.as_ref().map(|explicit_data_type| {
                    explicit_data_type
                        .perform_semantic_analysis(None, ctx)
                        .and_then(|_| explicit_data_type.kind.resolve(ctx, None))
                });

                let resolved_explicit_type = match resolved_explicit_type {
                    None => None,
                    Some(Some(data_type)) => Some(data_type),
                    Some(None) => {
                        let _ = value.perform_semantic_analysis(ctx, is_lhs, None);

                        pattern.kind.destructure_unknown(ctx);

                        return None;
                    }
                };

                let Some(value_type) = value.kind.infer_data_type(ctx) else {
                    let _ = value.perform_semantic_analysis(
                        ctx,
                        is_lhs,
                        resolved_explicit_type.as_ref(),
                    );

                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                let variable_type = match resolved_explicit_type {
                    None => value_type.clone(),
                    Some(data_type) => data_type,
                };

                let mut error = false;

                if value
                    .perform_semantic_analysis(ctx, is_lhs, Some(&variable_type))
                    .is_none()
                {
                    error = true;
                }

                if variable_type
                    .perform_equality_semantic_analysis(ctx, &value_type, value)
                    .is_none()
                {
                    error = true;
                }

                if pattern
                    .perform_irrefutablity_semantic_analysis(ctx)
                    .is_none()
                {
                    error = true;
                }

                if variable_type
                    .destructure_and_perform_semantic_analysis(ctx, value.span, pattern)
                    .is_none()
                {
                    pattern.kind.destructure_unknown(ctx);

                    error = true;
                }

                if error {
                    return None;
                }

                Some(())
            }
            StatementKind::While(expression, statement) => {
                let expression_result =
                    expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::Boolean));
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
                let expression_result =
                    expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::Boolean));
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
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs, None);

                let expression_type = expression.kind.infer_data_type(ctx)?;

                let Some(iterable_type) = expression_type.get_iterable_type() else {
                    ctx.declare_variable_unknown(name);

                    statement.perform_semantic_analysis(ctx, is_lhs);

                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotIterateType(expression_type),
                        ),
                    });
                };

                ctx.declare_variable_known(name, iterable_type);

                statement.perform_semantic_analysis(ctx, is_lhs)?;

                expression_result?;

                Some(())
            }
            StatementKind::Block(statements) => {
                ctx.scopes.push_front(Scope::default());

                let result = statements
                    .iter()
                    .map(|statement| statement.perform_semantic_analysis(ctx, is_lhs))
                    .all_some();

                ctx.scopes.pop_front();

                result
            }
            StatementKind::AppendData(target, value) => {
                let target_result = target.perform_semantic_analysis(
                    ctx,
                    is_lhs,
                    Some(&DataTypeKind::Data(Box::new(DataTypeKind::List(Box::new(
                        DataTypeKind::SNBT,
                    ))))),
                );
                let value_result = value.perform_semantic_analysis(ctx, is_lhs, None);

                target_result?;
                value_result?;

                Some(())
            }
            StatementKind::RemoveData(target) => target.perform_semantic_analysis(
                ctx,
                is_lhs,
                Some(&DataTypeKind::Data(Box::new(DataTypeKind::SNBT))),
            ),
            StatementKind::TypeDeclaration(name, generics, alias) => {
                if ctx.data_type_is_declared(name) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsAlreadyDefined(name.clone()),
                        ),
                    });
                }

                if alias
                    .perform_semantic_analysis(Some(generics), ctx)
                    .is_some()
                {
                    let alias = alias.kind.resolve(ctx, Some(generics)).unwrap();

                    ctx.declare_data_type(
                        name.clone(),
                        Some(DataTypeDeclarationKind::Alias {
                            name: name.clone(),
                            generics: generics.clone(),
                            alias,
                        }),
                    );

                    Some(())
                } else {
                    ctx.declare_data_type(name.clone(), None);

                    None
                }
            }
            StatementKind::StructDeclaration(name, generics, fields) => {
                if ctx.data_type_is_declared(name) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsAlreadyDefined(name.clone()),
                        ),
                    });
                }

                if fields
                    .values()
                    .map(|field| field.perform_semantic_analysis(Some(generics), ctx))
                    .all_some()
                    .is_none()
                {
                    ctx.declare_data_type(name.clone(), None);

                    return None;
                }

                let resolved_fields = fields
                    .iter()
                    .map(|(key, field)| {
                        field
                            .kind
                            .resolve(ctx, Some(generics))
                            .map(|data_type| (key.clone(), data_type))
                    })
                    .collect::<Option<BTreeMap<_, _>>>()?;

                ctx.declare_data_type(
                    name.clone(),
                    Some(DataTypeDeclarationKind::Struct {
                        name: name.clone(),
                        generics: generics.clone(),
                        fields: resolved_fields,
                    }),
                );

                Some(())
            }
        }
    }

    pub fn new(span: Span, kind: StatementKind) -> Statement {
        Statement { span, kind }
    }
}
