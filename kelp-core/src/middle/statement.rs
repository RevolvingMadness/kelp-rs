use std::collections::BTreeMap;

use crate::compile_context::{LoopInfo, LoopType};
use crate::low::expression::Expression as LowExpression;
use crate::middle::data_type::DataType;
use crate::middle::item::Item;
use crate::{
    compile_context::CompileContext, datapack::Datapack, middle::expression::Expression,
    pattern::Pattern,
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

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(DataType, Pattern, Expression),
    While(Expression, Box<Self>),
    Loop(Box<Self>),
    Match(Expression, BTreeMap<IntegerRange, Box<Self>>),
    If(Expression, Box<Self>, Option<Box<Self>>),
    ForIn(bool, String, Expression, Box<Self>),
    Block(Vec<Self>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
}

fn compile_if(
    datapack: &mut Datapack,
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

#[derive(Debug, Clone, Copy)]
pub enum ControlFlowKind {
    Break,
    Continue,
}

impl Statement {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Expression(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Let(data_type, pattern, value) => {
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

                body.compile(datapack, &mut while_body_ctx);

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

                body.compile(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForIn(is_reversed, variable_name, collection, body) => {
                let collection = collection.kind.resolve(datapack, ctx);

                let collection_data_type = collection.get_data_type().get_iterable_type().unwrap();

                if collection_data_type.equals(&DataType::String) {
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
                        DataType::Data(Box::new(DataType::SNBT)),
                        LowExpression::Data(Box::new((
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
                    body.compile(datapack, &mut for_body_ctx);
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
                        DataType::Data(Box::new(DataType::SNBT)),
                        LowExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
                    );
                    body.compile(datapack, &mut for_body_ctx);
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
                    statement.compile(datapack, ctx);
                }

                datapack.end_scope();
            }
            Self::Match(_, _) => {
                todo!()
            }
            Self::If(condition, body, else_body) => {
                let mut body_ctx = ctx.create_child_ctx();
                let control_flow = body.get_control_flow(ctx);
                body.compile(datapack, &mut body_ctx);

                let (invert, condition) = condition
                    .kind
                    .resolve(datapack, ctx)
                    .to_execute_condition(datapack, ctx, false)
                    .unwrap();

                if let Some(else_body) = else_body {
                    let mut else_body_ctx = ctx.create_child_ctx();
                    let else_control_flow = else_body.get_control_flow(ctx);
                    else_body.compile(datapack, &mut else_body_ctx);

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
            Self::Item(item) => item.compile(datapack, ctx),
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
                statement.get_control_flow_kind()
            }
            Self::Match(_, _) => todo!(),
            Self::If(_, statement, else_statement) => {
                statement.get_control_flow_kind().or_else(|| {
                    else_statement
                        .as_ref()
                        .and_then(|statement| statement.get_control_flow_kind())
                })
            }
            Self::Block(statements) => statements.iter().find_map(Self::get_control_flow_kind),
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
