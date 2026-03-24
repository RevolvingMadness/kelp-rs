use std::collections::HashMap;

use crate::compile_context::{LoopInfo, LoopType};
use crate::middle::data_type::DataType;
use crate::middle::item::Item;
use crate::middle::pattern::Pattern;
use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::expression::Expression as LowExpression, middle::expression::Expression,
};
use minecraft_command_types::command::Command;
use minecraft_command_types::command::data::{
    DataCommand, DataCommandModification, DataCommandModificationMode,
};
use minecraft_command_types::command::execute::{ExecuteIfSubcommand, ExecuteSubcommand};
use minecraft_command_types::command::r#return::ReturnCommand;
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode, SNBTCompound};
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
    Match(Expression, HashMap<IntegerRange, Box<Self>>),
    For(bool, Pattern, Expression, Box<Self>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
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

impl ControlFlowKind {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Break => "break",
            Self::Continue => "continue",
        }
    }
}

impl Statement {
    #[must_use]
    pub fn get_data_type(&self) -> Option<DataType> {
        match self {
            Self::Expression(expression) => Some(expression.data_type.clone()),
            Self::Let(_, _, _)
            | Self::While(_, _)
            | Self::Loop(_)
            | Self::Match(_, _)
            | Self::For(_, _, _, _)
            | Self::Append(_, _)
            | Self::Remove(_)
            | Self::Item(_)
            | Self::Break
            | Self::Continue => None,
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Expression(expression) => expression.kind.compile_as_statement(datapack, ctx),
            Self::Let(data_type, pattern, value) => {
                let value = value.kind.resolve(datapack, ctx);

                pattern.destructure(datapack, ctx, data_type, value);
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
                    type_: LoopType::While(should_be_inverted, Box::new(condition)),
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
            Self::For(is_reversed, pattern, iterable, body) => {
                let iterable_data_type = iterable.data_type.get_iterable_type().unwrap();

                let iterable = iterable.kind.resolve(datapack, ctx);

                if iterable_data_type.equals(&DataType::String) {
                    let (unique_data_target, unique_path, name) = datapack.get_unique_data_named();
                    let (unique_data_target_2, unique_path_2) = datapack.get_unique_data();

                    iterable.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    pattern.destructure(
                        datapack,
                        &mut for_body_ctx,
                        iterable_data_type,
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

                    let mut map = SNBTCompound::new();
                    map.insert(SNBTString(false, name), SNBT::macroable_string(""));
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

                    iterable.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    let unique_path = unique_path.with_node(NbtPathNode::Index(Some(
                        SNBT::macroable_integer(if is_reversed { -1 } else { 0 }),
                    )));

                    pattern.destructure(
                        datapack,
                        &mut for_body_ctx,
                        iterable_data_type,
                        LowExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
                    );

                    body.compile(datapack, &mut for_body_ctx);

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
            Self::Match(_, _) => {
                todo!()
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

    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Option<LowExpression> {
        match self {
            Self::Expression(expression) => Some(expression.kind.resolve(datapack, ctx)),
            Self::Let(data_type, pattern, value) => {
                let value = value.kind.resolve(datapack, ctx);

                pattern.destructure(datapack, ctx, data_type, value);

                None
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
                    type_: LoopType::While(should_be_inverted, Box::new(condition)),
                });

                body.compile(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);

                None
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

                None
            }
            Self::For(is_reversed, pattern, iterable, body) => {
                let iterable_data_type = iterable.data_type.get_iterable_type().unwrap();

                let iterable = iterable.kind.resolve(datapack, ctx);

                if iterable_data_type.equals(&DataType::String) {
                    let (unique_data_target, unique_path, name) = datapack.get_unique_data_named();
                    let (unique_data_target_2, unique_path_2) = datapack.get_unique_data();

                    iterable.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    pattern.destructure(
                        datapack,
                        &mut for_body_ctx,
                        iterable_data_type,
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

                    let mut map = SNBTCompound::new();
                    map.insert(SNBTString(false, name), SNBT::macroable_string(""));
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

                    iterable.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    let unique_path = unique_path.with_node(NbtPathNode::Index(Some(
                        SNBT::macroable_integer(if is_reversed { -1 } else { 0 }),
                    )));

                    pattern.destructure(
                        datapack,
                        &mut for_body_ctx,
                        iterable_data_type,
                        LowExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
                    );

                    body.compile(datapack, &mut for_body_ctx);

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

                None
            }
            Self::Match(_, _) => {
                todo!()
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

                None
            }
            Self::Remove(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Remove(target.target, path)),
                );

                None
            }
            Self::Break => {
                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                None
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

                None
            }
            Self::Item(item) => {
                item.compile(datapack, ctx);

                None
            }
        }
    }
}
