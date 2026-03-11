use std::collections::BTreeMap;

use crate::compile_context::{LoopInfo, LoopType};
use crate::expression::constant::ResolvedExpression;
use crate::item::Item;
use crate::span::Span;
use crate::trait_ext::OptionUnitIterExt;
use crate::{
    compile_context::CompileContext,
    data_type::{DataTypeKind, high::HighDataType},
    datapack::HighDatapack,
    expression::Expression,
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
use minecraft_command_types::command::enums::store_type::StoreType;
use minecraft_command_types::command::execute::{
    ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
};
use minecraft_command_types::command::r#return::ReturnCommand;
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::{SNBT, SNBTString};
use nonempty::nonempty;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Let(Option<HighDataType>, Pattern, Expression),
    While(Expression, Box<Statement>),
    Loop(Box<Statement>),
    Match(Expression, BTreeMap<IntegerRange, Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    ForIn(bool, String, Expression, Box<Statement>),
    Block(Vec<Statement>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
}

fn compile_if(
    datapack: &mut HighDatapack,
    caller_ctx: &mut CompileContext,
    control_flow: Option<ControlFlow>,
    mut body_ctx: CompileContext,
    inverted: bool,
    condition: ExecuteIfSubcommand,
) {
    let should_inine = body_ctx.num_commands() <= 5;

    if should_inine {
        for command in body_ctx.commands {
            caller_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    inverted,
                    condition
                        .clone()
                        .then(ExecuteSubcommand::Run(Box::new(command))),
                )),
            );
        }
    } else {
        let body_paths = datapack.get_unique_function_paths();
        let body_function_resource_location = ResourceLocation::new_namespace_paths(
            datapack.current_namespace_name(),
            body_paths.clone(),
        );

        match control_flow {
            Some(control_flow) => match control_flow.kind {
                ControlFlowKind::Break | ControlFlowKind::Continue => {
                    caller_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            inverted,
                            condition.then(ExecuteSubcommand::If(
                                true,
                                ExecuteIfSubcommand::Function(
                                    body_function_resource_location,
                                    Box::new(ExecuteSubcommand::Run(Box::new(Command::Return(
                                        ReturnCommand::Fail,
                                    )))),
                                ),
                            )),
                        )),
                    );
                }
            },
            None => {
                caller_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(inverted, condition).then(
                        ExecuteSubcommand::Run(Box::new(Command::Function(
                            body_function_resource_location,
                            None,
                        ))),
                    )),
                );
            }
        }

        body_ctx.add_command(datapack, Command::Return(ReturnCommand::Value(1)));

        datapack.compile_and_add_to_function(&body_paths, &mut body_ctx);
    }
}

#[derive(Debug, Clone)]
pub struct ControlFlow {
    pub kind: ControlFlowKind,
    pub loop_info: LoopInfo,
}

#[derive(Debug, Clone)]
pub enum ControlFlowKind {
    Break,
    Continue,
}

impl StatementKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Statement {
        Statement { span, kind: self }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            Self::Expression(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Let(data_type, pattern, value) => {
                #[allow(clippy::map_unwrap_or)]
                let data_type = data_type
                    .map(|data_type| data_type.kind.resolve(datapack, None).unwrap())
                    .unwrap_or_else(|| value.kind.infer_data_type(datapack).unwrap());

                let value = value.kind.resolve(datapack, ctx);

                pattern.kind.destructure(datapack, ctx, data_type, value);
            }
            Self::While(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (should_be_inverted, condition) = condition
                    .kind
                    .resolve(datapack, &mut condition_ctx)
                    .to_execute_condition(datapack, &mut condition_ctx, false)
                    .unwrap();

                condition_ctx.add_command(
                    datapack,
                    Command::Execute(
                        ExecuteSubcommand::If(should_be_inverted, condition.clone()).then(
                            ExecuteSubcommand::Run(Box::new(Command::Function(
                                while_function_resource_location.clone(),
                                None,
                            ))),
                        ),
                    ),
                );

                let mut while_body_ctx = ctx.create_child_ctx();
                while_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: while_function_resource_location,
                    type_: LoopType::While(should_be_inverted, condition),
                });

                body.kind.compile(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);
            }
            Self::Loop(body) => {
                let loop_function_paths = datapack.get_unique_function_paths();
                let loop_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    loop_function_paths.clone(),
                );

                let iteration_command =
                    Command::Function(loop_function_resource_location.clone(), None);

                let mut loop_body_ctx = ctx.create_child_ctx();
                loop_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: loop_function_resource_location,
                    type_: LoopType::Loop,
                });

                body.kind.compile(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForIn(is_reversed, variable_name, collection, body) => {
                let collection_data_type = collection
                    .kind
                    .infer_data_type(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap_or_else(|| panic!("Expression {:?} is not iterable", collection));

                let collection = collection.kind.resolve(datapack, ctx);

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
                        ResolvedExpression::Data(Box::new((
                            unique_data_target_2.clone(),
                            unique_path_2.clone(),
                        ))),
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
                                Some(unique_path),
                                Some(i32::from(!is_reversed)),
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
                        ResolvedExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
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
            Self::Block(body) => {
                datapack.start_scope();

                for statement in body {
                    statement.kind.compile(datapack, ctx);
                }

                datapack.end_scope();
            }
            Self::Match(_, _) => {
                todo!()
            }
            Self::If(condition, body, else_body) => {
                let mut body_ctx = ctx.create_child_ctx();
                let control_flow = body.kind.get_control_flow(ctx);
                body.kind.compile(datapack, &mut body_ctx);

                let (invert, condition) = condition
                    .kind
                    .resolve(datapack, ctx)
                    .to_execute_condition(datapack, ctx, false)
                    .unwrap();

                if let Some(else_body) = else_body {
                    let mut else_body_ctx = ctx.create_child_ctx();
                    let else_control_flow = else_body.kind.get_control_flow(ctx);
                    else_body.kind.compile(datapack, &mut else_body_ctx);

                    let should_add_condition =
                        body_ctx.num_commands() > 1 || else_body_ctx.num_commands() > 1;

                    let (invert, condition) = if should_add_condition {
                        let unique_score = datapack.get_unique_score();

                        ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::Store(
                                StoreType::Result,
                                ExecuteStoreSubcommand::Score(
                                    unique_score.score.clone(),
                                    Box::new(ExecuteSubcommand::If(invert, condition)),
                                ),
                            )),
                        );

                        (
                            true,
                            ExecuteIfSubcommand::Score(
                                unique_score.score,
                                ScoreComparison::Range(IntegerRange::new_single(0)),
                                None,
                            ),
                        )
                    } else {
                        (invert, condition)
                    };

                    compile_if(
                        datapack,
                        ctx,
                        control_flow,
                        body_ctx,
                        invert,
                        condition.clone(),
                    );

                    compile_if(
                        datapack,
                        ctx,
                        else_control_flow,
                        else_body_ctx,
                        !invert,
                        condition,
                    );
                } else {
                    let should_add_condition = body_ctx.num_commands() > 1;

                    let (invert, condition) = if should_add_condition {
                        let unique_score = datapack.get_unique_score();

                        ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::Store(
                                StoreType::Result,
                                ExecuteStoreSubcommand::Score(
                                    unique_score.score.clone(),
                                    Box::new(ExecuteSubcommand::If(invert, condition)),
                                ),
                            )),
                        );

                        (
                            true,
                            ExecuteIfSubcommand::Score(
                                unique_score.score,
                                ScoreComparison::Range(IntegerRange::new_single(0)),
                                None,
                            ),
                        )
                    } else {
                        (invert, condition)
                    };

                    compile_if(datapack, ctx, control_flow, body_ctx, invert, condition);
                }
            }
            Self::Append(target, value) => {
                let target = target.kind.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                let (target, path) = target.as_data(datapack, ctx, false);

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
            Self::Remove(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Remove(target.target, path)),
                );
            }
            Self::Break => {
                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));
            }
            Self::Continue => {
                let LoopInfo {
                    resource_location: loop_resource_location,
                    type_: loop_type,
                } = ctx.loop_info.as_ref().unwrap().clone();

                let iteration_command = Command::Function(loop_resource_location, None);

                let command = match loop_type {
                    LoopType::While(invert, condition) => Command::Execute(ExecuteSubcommand::If(
                        invert,
                        condition.then(ExecuteSubcommand::Run(Box::new(iteration_command))),
                    )),
                    LoopType::Loop => iteration_command,
                };

                ctx.add_command(
                    datapack,
                    Command::Return(ReturnCommand::Run(Box::new(command))),
                );
            }
            Self::Item(item) => item.kind.compile(datapack, ctx),
        }
    }

    #[must_use]
    pub fn get_control_flow_kind(&self) -> Option<ControlFlowKind> {
        match self {
            Self::Expression(_)
            | Self::Let(_, _, _)
            | Self::Append(_, _)
            | Self::Remove(_)
            | Self::Item(_) => None,
            Self::While(_, statement) | Self::Loop(statement) | Self::ForIn(_, _, _, statement) => {
                statement.kind.get_control_flow_kind()
            }
            Self::Match(_, _) => todo!(),
            Self::If(_, statement, else_statement) => {
                statement.kind.get_control_flow_kind().or_else(|| {
                    else_statement
                        .as_ref()
                        .and_then(|statement| statement.kind.get_control_flow_kind())
                })
            }
            Self::Block(statements) => statements
                .iter()
                .find_map(|statement| statement.kind.get_control_flow_kind()),
            Self::Break => Some(ControlFlowKind::Break),
            Self::Continue => Some(ControlFlowKind::Continue),
        }
    }

    #[must_use]
    pub fn get_control_flow(&self, ctx: &mut CompileContext) -> Option<ControlFlow> {
        let loop_info = ctx.loop_info.as_ref()?.clone();

        Some(ControlFlow {
            kind: self.get_control_flow_kind()?,
            loop_info,
        })
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
            StatementKind::Expression(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }
            StatementKind::Let(explicit_type, pattern, value) => {
                let variable_type = if let Some(explicit_type) = explicit_type {
                    if explicit_type.perform_semantic_analysis(None, ctx).is_none() {
                        let value_result = value.perform_semantic_analysis(ctx, is_lhs, None);

                        pattern.kind.destructure_unknown(ctx);

                        value_result?;

                        return None;
                    }

                    let explicit_type = explicit_type.kind.resolve(ctx, None).unwrap();

                    let value_result =
                        value.perform_semantic_analysis(ctx, is_lhs, Some(&explicit_type));

                    pattern.kind.destructure_unknown(ctx);

                    value_result?;

                    let value_type = value.kind.infer_data_type(ctx);

                    explicit_type.try_infer(value_type)
                } else {
                    let value_result = value.perform_semantic_analysis(ctx, is_lhs, None);

                    pattern.kind.destructure_unknown(ctx);

                    value_result?;

                    value.kind.infer_data_type(ctx)?
                };

                let mut error = false;

                if value
                    .perform_semantic_analysis(ctx, is_lhs, Some(&variable_type))
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
            StatementKind::While(condition, body) => {
                let condition_result =
                    condition.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::Boolean));

                ctx.loop_depth += 1;
                let body_result = body.perform_semantic_analysis(ctx, is_lhs);
                ctx.loop_depth -= 1;

                condition_result?;

                let expression_type = condition.kind.infer_data_type(ctx)?;

                if !expression_type.is_condition() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: condition.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotCondition(expression_type),
                        ),
                    });
                }

                body_result?;

                Some(())
            }
            StatementKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body_result = body.perform_semantic_analysis(ctx, is_lhs);
                ctx.loop_depth -= 1;

                body_result?;

                Some(())
            }
            StatementKind::Match(_, _) => todo!(),
            StatementKind::If(expression, statement, else_statement) => {
                let expression_result =
                    expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::Boolean));
                let statement_result = statement.perform_semantic_analysis(ctx, is_lhs);
                let else_statement_result =
                    else_statement.as_ref().map_or(Some(()), |else_statement| {
                        else_statement.perform_semantic_analysis(ctx, is_lhs)
                    });

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
            StatementKind::Append(target, value) => {
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
            StatementKind::Remove(target) => target.perform_semantic_analysis(
                ctx,
                is_lhs,
                Some(&DataTypeKind::Data(Box::new(DataTypeKind::SNBT))),
            ),
            StatementKind::Break => {
                if ctx.loop_depth == 0 {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Break),
                        ),
                    })
                } else {
                    Some(())
                }
            }
            StatementKind::Continue => {
                if ctx.loop_depth == 0 {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Continue),
                        ),
                    })
                } else {
                    Some(())
                }
            }
            StatementKind::Item(item) => item.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    #[must_use]
    pub const fn new(span: Span, kind: StatementKind) -> Self {
        Self { span, kind }
    }
}
