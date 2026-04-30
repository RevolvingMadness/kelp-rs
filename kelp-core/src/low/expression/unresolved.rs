use std::collections::HashMap;

use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode},
        enums::store_type::StoreType,
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
        },
        r#return::ReturnCommand,
    },
    nbt_path::{NbtPath as LowNbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use nonempty::nonempty;
use ordered_float::NotNan;

use crate::{
    compile_context::{CompileContext, LoopInfo, LoopType},
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::{
        data::DataTarget,
        data_type::DataType,
        entity_selector::EntitySelector,
        environment::{
            r#type::r#struct::{StructStructId, TupleStructId},
            value::ValueId,
        },
        expression::{
            command::{
                Command as MiddleCommand,
                execute::subcommand::r#if::ExecuteIfSubcommand as MiddleExecuteIfSubcommand,
            },
            resolved::ResolvedExpression,
        },
        nbt_path::NbtPath,
        pattern::Pattern,
        player_score::PlayerScore,
        statement::{ControlFlow, ControlFlowKind, Statement},
        supports_expression_sigil::SupportsExpressionSigil,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::Place,
    runtime_storage_type::RuntimeStorageType,
};

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

fn compile_if_internal(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    condition: UnresolvedExpression,
    body: UnresolvedExpression,
    else_body: Option<Box<UnresolvedExpression>>,
    output: Option<(GeneratedDataTarget, LowNbtPath)>,
) {
    let mut body_ctx = ctx.create_child_ctx();
    let control_flow = body.kind.get_control_flow(ctx);
    let body = body.kind.resolve(datapack, &mut body_ctx);

    if let Some((output_target, output_path)) = &output {
        body.assign_to_data(
            datapack,
            &mut body_ctx,
            output_target.clone(),
            output_path.clone(),
        );
    }

    let (invert, condition) = condition
        .kind
        .resolve(datapack, ctx)
        .to_execute_condition(datapack, ctx, false)
        .unwrap();

    if let Some(else_body) = else_body {
        let mut else_body_ctx = ctx.create_child_ctx();
        let else_control_flow = else_body.kind.get_control_flow(ctx);
        let else_body = else_body.kind.resolve(datapack, &mut else_body_ctx);

        if let Some((output_target, output_path)) = output {
            else_body.assign_to_data(datapack, &mut else_body_ctx, output_target, output_path);
        }

        let should_add_condition = body_ctx.num_commands() > 1 || else_body_ctx.num_commands() > 1;

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

#[derive(Debug, Clone)]
pub enum UnresolvedExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    InferredInteger(i32),
    Long(i64),
    Float(NotNan<f32>),
    InferredFloat(NotNan<f32>),
    Double(NotNan<f64>),
    String(SNBTString),
    Unit,
    Underscore,
    Unary(UnaryOperator, Box<UnresolvedExpression>),
    Arithmetic(
        Box<UnresolvedExpression>,
        ArithmeticOperator,
        Box<UnresolvedExpression>,
    ),
    Comparison(
        Box<UnresolvedExpression>,
        ComparisonOperator,
        Box<UnresolvedExpression>,
    ),
    Logical(
        Box<UnresolvedExpression>,
        LogicalOperator,
        Box<UnresolvedExpression>,
    ),
    AugmentedAssignment(
        Box<UnresolvedExpression>,
        ArithmeticOperator,
        Box<UnresolvedExpression>,
    ),
    Assignment(Box<UnresolvedExpression>, Box<UnresolvedExpression>),
    List(Vec<UnresolvedExpression>),
    Compound(HashMap<SNBTString, UnresolvedExpression>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),
    Condition(bool, Box<MiddleExecuteIfSubcommand>),
    Command(Box<MiddleCommand>),
    Index(Box<UnresolvedExpression>, Box<UnresolvedExpression>),
    FieldAccess(Box<UnresolvedExpression>, SNBTString),
    AsCast(Box<UnresolvedExpression>, DataType),
    ToCast(
        Option<NotNan<f32>>,
        Box<UnresolvedExpression>,
        RuntimeStorageType,
    ),
    Tuple(Vec<UnresolvedExpression>),
    Variable(ValueId),
    StructStruct(StructStructId, HashMap<SNBTString, UnresolvedExpression>),
    TupleStruct(TupleStructId, Vec<UnresolvedExpression>),
    If(
        Box<UnresolvedExpression>,
        Box<UnresolvedExpression>,
        Option<Box<UnresolvedExpression>>,
    ),
    Block(Vec<Statement>, Option<Box<UnresolvedExpression>>),
    WhileLoop(Box<UnresolvedExpression>, Box<UnresolvedExpression>),
    Loop(Box<UnresolvedExpression>),
    ForLoop(
        bool,
        Box<Pattern>,
        Box<UnresolvedExpression>,
        Box<UnresolvedExpression>,
    ),
    ResourceLocation(Box<SupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<SupportsExpressionSigil<EntitySelector>>),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl UnresolvedExpressionKind {
    #[must_use]
    pub const fn with(self, data_type: DataType) -> UnresolvedExpression {
        UnresolvedExpression {
            kind: self,
            data_type,
        }
    }

    #[must_use]
    pub fn get_control_flow_kind(&self) -> Option<ControlFlowKind> {
        match self {
            Self::If(_, body, else_body) => body.kind.get_control_flow_kind().or_else(|| {
                else_body
                    .as_ref()
                    .and_then(|else_body| else_body.kind.get_control_flow_kind())
            }),

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if let Some(kind) = statement.get_control_flow_kind() {
                        return Some(kind);
                    }
                }

                if let Some(tail_expression) = tail_expression
                    && let Some(kind) = tail_expression.kind.get_control_flow_kind()
                {
                    return Some(kind);
                }

                None
            }

            Self::WhileLoop(_, body) => body.kind.get_control_flow_kind(),
            Self::Loop(body) => body.kind.get_control_flow_kind(),
            Self::ForLoop(_, _, _, body) => body.kind.get_control_flow_kind(),

            _ => None,
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

    #[must_use]
    pub const fn can_be_referenced(&self) -> bool {
        matches!(
            self,
            Self::Unary(UnaryOperator::Dereference, _)
                | Self::PlayerScore(_)
                | Self::Data(_)
                | Self::Index(_, _)
                | Self::FieldAccess(_, _)
                | Self::Variable(_)
        )
    }

    #[must_use]
    pub const fn is_index_out_of_bounds(&self, index: &UnresolvedExpression) -> Option<bool> {
        let Self::List(expressions) = self else {
            return None;
        };

        let UnresolvedExpression {
            kind: Self::Integer(index) | Self::InferredInteger(index),
            ..
        } = index
        else {
            return None;
        };

        Some((*index as usize) >= expressions.len())
    }

    pub fn as_place(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Option<Place> {
        match self {
            Self::Underscore => Some(Place::Underscore),

            Self::Unary(operator, expression) => match operator {
                UnaryOperator::Negate | UnaryOperator::Reference | UnaryOperator::Invert => None,
                UnaryOperator::Dereference => {
                    let expression = expression.kind.as_place(datapack, ctx)?;

                    Some(Place::Dereference(Box::new(expression)))
                }
            },
            Self::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                Some(Place::Score(score))
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Some(Place::Data(target, path))
            }
            Self::Index(target, index) => {
                let target = target.kind.resolve(datapack, ctx);
                let index = index.kind.resolve(datapack, ctx);

                let index_result = target.index(datapack, ctx, index).unwrap();
                let place = index_result.as_place()?;

                Some(place)
            }
            Self::FieldAccess(target, field) => {
                let data_type = &target.data_type;

                let target = target.kind.resolve(datapack, ctx);

                let field_result = target.access_field(data_type, datapack, &field.1).unwrap();
                let place = field_result.as_place()?;

                Some(place)
            }
            Self::Tuple(tuple) => Some(Place::Tuple(
                tuple
                    .into_iter()
                    .map(|expression| expression.kind.as_place(datapack, ctx))
                    .collect::<Option<_>>()?,
            )),
            Self::Variable(name) => Some(Place::Value(name)),
            _ => None,
        }
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ResolvedExpression {
        match self {
            Self::Unary(unary_operator, expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                match unary_operator {
                    UnaryOperator::Negate => expression.negate(datapack, ctx),
                    UnaryOperator::Invert => expression.invert(),
                    UnaryOperator::Reference => expression,
                    UnaryOperator::Dereference => expression.dereference(datapack, ctx).unwrap(),
                }
            }
            Self::Arithmetic(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_arithmetic(datapack, ctx, operator, right)
            }
            Self::Comparison(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_comparison(datapack, ctx, operator, right)
            }
            Self::Logical(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_logical_operation(datapack, ctx, operator, right)
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let value = value.kind.resolve(datapack, ctx);

                let target_place = target.kind.as_place(datapack, ctx).unwrap();
                target_place.augmented_assign(datapack, ctx, operator, value);

                ResolvedExpression::Unit
            }
            Self::Assignment(target, value) => {
                let target = target.kind.as_place(datapack, ctx).unwrap();
                let value = value.kind.resolve(datapack, ctx);

                target.assign(datapack, ctx, value);

                ResolvedExpression::Unit
            }
            Self::List(expressions) => ResolvedExpression::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.kind.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            Self::Compound(compound) => ResolvedExpression::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.kind.resolve(datapack, ctx)))
                    .collect::<HashMap<_, _>>(),
            ),
            Self::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                ResolvedExpression::PlayerScore(score)
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                ResolvedExpression::Data(Box::new((target, path)))
            }
            Self::Condition(inverted, condition) => {
                let condition = condition.compile(datapack, ctx);

                ResolvedExpression::Condition(inverted, Box::new(condition))
            }
            Self::Command(command) => {
                let command = command.compile(datapack, ctx);

                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command))),
                        ),
                    )),
                );

                ResolvedExpression::PlayerScore(unique_score)
            }
            Self::Index(target, index) => {
                let target = target.kind.resolve(datapack, ctx);
                let index = index.kind.resolve(datapack, ctx);

                target.index(datapack, ctx, index).unwrap()
            }
            Self::FieldAccess(target, field) => {
                let data_type = target.data_type;

                let target = target.kind.resolve(datapack, ctx);

                target.access_field(&data_type, datapack, &field.1).unwrap()
            }
            Self::AsCast(expression, data_type) => {
                let expression = expression.kind.resolve(datapack, ctx);

                expression.cast_to(data_type)
            }
            Self::ToCast(scale, expression, runtime_storage_type) => {
                let expression = expression.kind.resolve(datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if let Some(scale) = scale {
                            expression.to_score_scale(datapack, ctx, scale)
                        } else {
                            expression.to_score(datapack, ctx)
                        }
                    }
                    RuntimeStorageType::Data => {
                        let (unique_target, unique_path) = if let Some(scale) = scale {
                            expression.as_data_scale(datapack, ctx, scale)
                        } else {
                            expression.as_data(datapack, ctx, true)
                        };

                        ResolvedExpression::Data(Box::new((unique_target, unique_path)))
                    }
                }
            }
            Self::Tuple(expressions) => ResolvedExpression::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.resolve(datapack, ctx))
                    .collect(),
            ),
            Self::StructStruct(id, fields) => ResolvedExpression::StructStruct(
                id,
                fields
                    .into_iter()
                    .map(|(key, field)| (key.1, field.kind.resolve(datapack, ctx)))
                    .collect(),
            ),
            Self::TupleStruct(id, fields) => ResolvedExpression::TupleStruct(
                id,
                fields
                    .into_iter()
                    .map(|field| field.kind.resolve(datapack, ctx))
                    .collect(),
            ),
            Self::Underscore => unreachable!(),
            Self::Boolean(value) => ResolvedExpression::Boolean(value),
            Self::Byte(value) => ResolvedExpression::Byte(value),
            Self::Short(value) => ResolvedExpression::Short(value),
            Self::Integer(value) | Self::InferredInteger(value) => {
                ResolvedExpression::Integer(value)
            }
            Self::Long(value) => ResolvedExpression::Long(value),
            Self::Float(value) | Self::InferredFloat(value) => ResolvedExpression::Float(value),
            Self::Double(value) => ResolvedExpression::Double(value),
            Self::String(value) => ResolvedExpression::String(value),
            Self::Unit => ResolvedExpression::Unit,
            Self::Variable(id) => datapack.get_variable_value(id).1.clone(),
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    statement.compile_as_statement(datapack, ctx);
                }

                tail_expression.map_or(ResolvedExpression::Unit, |tail_expression| {
                    tail_expression.kind.resolve(datapack, ctx)
                })
            }
            Self::If(condition, body, else_body) => {
                let (output_target, output_path) = datapack.get_unique_data();

                compile_if_internal(
                    datapack,
                    ctx,
                    *condition,
                    *body,
                    else_body,
                    Some((output_target.clone(), output_path.clone())),
                );

                ResolvedExpression::Data(Box::new((output_target, output_path)))
            }
            Self::WhileLoop(condition, body) => {
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

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);

                ResolvedExpression::Unit
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
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
                    let unique_path = LowNbtPath(nonempty![NbtPathNode::RootCompound(map)]);

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
            Self::ResourceLocation(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                ResolvedExpression::ResourceLocation(resource_location)
            }
            Self::EntitySelector(selector) => {
                let selector = selector.compile(datapack, ctx);

                ResolvedExpression::EntitySelector(selector)
            }
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Assignment(target, value) => {
                let target = target.kind.as_place(datapack, ctx).unwrap();
                let value = value.kind.resolve(datapack, ctx);

                target.assign(datapack, ctx, value);
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let value = value.kind.resolve(datapack, ctx);

                let target_place = target.kind.as_place(datapack, ctx).unwrap();

                target_place.augmented_assign(datapack, ctx, operator, value);
            }
            Self::List(list) => {
                for element in list {
                    element.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.into_values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Condition(_, condition) => {
                let condition = condition.compile(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(false, condition)),
                );
            }
            Self::Command(command) => {
                let command = command.compile(datapack, ctx);

                ctx.add_command(datapack, command);
            }
            Self::AsCast(expression, _) | Self::ToCast(_, expression, _) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Tuple(tuple) => {
                for element in tuple {
                    element.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::StructStruct(_, field_expressions) => {
                for field_expression in field_expressions.into_values() {
                    field_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::TupleStruct(_, field_expressions) => {
                for field_expression in field_expressions {
                    field_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    statement.compile_as_statement(datapack, ctx);
                }

                if let Some(tail_expression) = tail_expression {
                    tail_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::If(condition, body, else_body) => {
                compile_if_internal(datapack, ctx, *condition, *body, else_body, None);
            }
            Self::WhileLoop(condition, body) => {
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

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
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
                    let unique_path = LowNbtPath(nonempty![NbtPathNode::RootCompound(map)]);

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
            Self::Underscore => {
                #[cfg(debug_assertions)]
                unreachable!();
            }
            Self::EntitySelector(selector) => {
                selector.compile_as_statement(datapack, ctx);
            }
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::InferredInteger(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::InferredFloat(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Unit
            | Self::Unary(_, _)
            | Self::Arithmetic(_, _, _)
            | Self::Comparison(_, _, _)
            | Self::Logical(_, _, _)
            | Self::PlayerScore(_)
            | Self::Data(_)
            | Self::Index(_, _)
            | Self::FieldAccess(_, _)
            | Self::ResourceLocation(_)
            | Self::Variable(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedExpression {
    pub kind: UnresolvedExpressionKind,
    pub data_type: DataType,
}
