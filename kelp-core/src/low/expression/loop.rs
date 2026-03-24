use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode},
        execute::{ExecuteIfSubcommand, ExecuteSubcommand},
    },
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use nonempty::nonempty;

use crate::{
    compile_context::{CompileContext, LoopInfo, LoopType},
    datapack::Datapack,
    low::{
        data_type::DataType,
        expression::{resolved::ResolvedExpression, unresolved::UnresolvedExpression},
        pattern::Pattern,
        statement::ControlFlowKind,
    },
};

#[derive(Debug, Clone)]
pub enum LoopExpressionKind {
    Predicate(Box<UnresolvedExpression>, Box<UnresolvedExpression>),
    Infinite(Box<UnresolvedExpression>),
    Iterator(
        bool,
        Box<Pattern>,
        Box<UnresolvedExpression>,
        Box<UnresolvedExpression>,
    ),
}

impl LoopExpressionKind {
    #[must_use]
    pub const fn with(self, data_type: DataType) -> LoopExpression {
        LoopExpression {
            kind: self,
            data_type,
        }
    }

    #[must_use]
    pub fn body(&self) -> &UnresolvedExpression {
        match self {
            Self::Predicate(_, body) | Self::Infinite(body) | Self::Iterator(_, _, _, body) => body,
        }
    }

    #[inline]
    #[must_use]
    pub fn get_control_flow_kind(&self) -> Option<ControlFlowKind> {
        self.body().kind.get_control_flow_kind()
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ResolvedExpression {
        match self {
            Self::Predicate(condition, body) => {
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

                body.kind
                    .compile_as_statement(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);

                ResolvedExpression::Unit
            }
            Self::Infinite(body) => {
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

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);

                ResolvedExpression::Unit
            }
            Self::Iterator(is_reversed, pattern, iterable, body) => {
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

                    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

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
                        ResolvedExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
                    );

                    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

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

                ResolvedExpression::Unit
            }
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Predicate(condition, body) => {
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

                body.kind
                    .compile_as_statement(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);
            }
            Self::Infinite(body) => {
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

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::Iterator(is_reversed, pattern, iterable, body) => {
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
                    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

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
                        ResolvedExpression::Data(Box::new((
                            unique_data_target.clone(),
                            unique_path.clone(),
                        ))),
                    );

                    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

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
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoopExpression {
    pub data_type: DataType,
    pub kind: LoopExpressionKind,
}
