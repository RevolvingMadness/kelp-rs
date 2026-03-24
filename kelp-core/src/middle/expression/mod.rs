use std::collections::HashMap;

use minecraft_command_types::{
    command::{
        Command,
        enums::store_type::StoreType,
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
        },
        r#return::ReturnCommand,
    },
    nbt_path::NbtPath as LowNbtPath,
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::SNBTString,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::expression::Expression as LowExpression,
    middle::{
        data::DataTarget,
        data_type::DataType,
        environment::{
            r#type::r#struct::{StructStructId, TupleStructId},
            value::ValueId,
        },
        expression::{
            command::{
                Command as MiddleCommand,
                execute::subcommand::r#if::ExecuteIfSubcommand as MiddleExecuteIfSubcommand,
            },
            r#loop::LoopExpression,
        },
        nbt_path::NbtPath,
        player_score::PlayerScore,
        statement::{ControlFlow, ControlFlowKind, Statement},
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::Place,
    runtime_storage_type::RuntimeStorageType,
};

pub mod command;
pub mod r#loop;

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
    condition: Expression,
    body: Expression,
    else_body: Option<Box<Expression>>,
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
pub enum ExpressionKind {
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
    Unary(UnaryOperator, Box<Expression>),
    Arithmetic(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    AugmentedAssignment(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
    Compound(HashMap<SNBTString, Expression>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),
    Condition(bool, Box<MiddleExecuteIfSubcommand>),
    Command(Box<MiddleCommand>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, SNBTString),
    AsCast(Box<Expression>, DataType),
    ToCast(Option<NotNan<f32>>, Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Variable(ValueId),
    StructStruct(StructStructId, HashMap<SNBTString, Expression>),
    TupleStruct(TupleStructId, Vec<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Block(Vec<Statement>),
    Loop(LoopExpression),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    #[must_use]
    pub const fn with(self, data_type: DataType) -> Expression {
        Expression {
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

            Self::Block(statements) => {
                for statement in statements {
                    if let Some(kind) = statement.get_control_flow_kind() {
                        return Some(kind);
                    }
                }

                None
            }

            Self::Loop(expression) => expression.kind.get_control_flow_kind(),

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
    pub const fn is_index_out_of_bounds(&self, index: &Expression) -> Option<bool> {
        let Self::List(expressions) = self else {
            return None;
        };

        let Expression {
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
                let target = target.kind.resolve(datapack, ctx).as_place()?;
                let index = index.kind.resolve(datapack, ctx);

                Some(Place::Index(Box::new(target), Box::new(index)))
            }
            Self::FieldAccess(target, field) => {
                let target = target.kind.resolve(datapack, ctx).as_place()?;

                Some(Place::Field(Box::new(target), field.1))
            }
            Self::Tuple(tuple) => Some(Place::Tuple(
                tuple
                    .into_iter()
                    .map(|expression| expression.kind.as_place(datapack, ctx))
                    .collect::<Option<_>>()?,
            )),
            Self::Variable(name) => Some(Place::Value(name)),
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
            | Self::Arithmetic(_, _, _)
            | Self::Comparison(_, _, _)
            | Self::Logical(_, _, _)
            | Self::AugmentedAssignment(_, _, _)
            | Self::Assignment(_, _)
            | Self::List(_)
            | Self::Compound(_)
            | Self::Condition(_, _)
            | Self::Command(_)
            | Self::AsCast(_, _)
            | Self::ToCast(_, _, _)
            | Self::StructStruct(_, _)
            | Self::TupleStruct(_, _)
            | Self::Block(_)
            | Self::If(_, _, _)
            | Self::Loop(_) => None,
        }
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowExpression {
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

                LowExpression::Unit
            }
            Self::Assignment(target, value) => {
                let target = target.kind.as_place(datapack, ctx).unwrap();
                let value = value.kind.resolve(datapack, ctx);

                target.assign(datapack, ctx, value);

                LowExpression::Unit
            }
            Self::List(expressions) => LowExpression::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.kind.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            Self::Compound(compound) => LowExpression::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.kind.resolve(datapack, ctx)))
                    .collect::<HashMap<_, _>>(),
            ),
            Self::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                LowExpression::PlayerScore(score)
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                LowExpression::Data(Box::new((target, path)))
            }
            Self::Condition(inverted, condition) => {
                let condition = condition.compile(datapack, ctx);

                LowExpression::Condition(inverted, Box::new(condition))
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

                LowExpression::PlayerScore(unique_score)
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

                        LowExpression::Data(Box::new((unique_target, unique_path)))
                    }
                }
            }
            Self::Tuple(expressions) => LowExpression::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.resolve(datapack, ctx))
                    .collect(),
            ),
            Self::StructStruct(id, fields) => LowExpression::StructStruct(
                id,
                fields
                    .into_iter()
                    .map(|(key, field)| (key.1, field.kind.resolve(datapack, ctx)))
                    .collect(),
            ),
            Self::TupleStruct(id, fields) => LowExpression::TupleStruct(
                id,
                fields
                    .into_iter()
                    .map(|field| field.kind.resolve(datapack, ctx))
                    .collect(),
            ),
            Self::Underscore => unreachable!(),
            Self::Boolean(value) => LowExpression::Boolean(value),
            Self::Byte(value) => LowExpression::Byte(value),
            Self::Short(value) => LowExpression::Short(value),
            Self::Integer(value) | Self::InferredInteger(value) => LowExpression::Integer(value),
            Self::Long(value) => LowExpression::Long(value),
            Self::Float(value) | Self::InferredFloat(value) => LowExpression::Float(value),
            Self::Double(value) => LowExpression::Double(value),
            Self::String(value) => LowExpression::String(value),
            Self::Unit => LowExpression::Unit,
            Self::Variable(id) => datapack.get_variable_value(id).1.clone(),
            Self::Block(mut statements) => {
                let last = statements.pop();

                for statement in statements {
                    statement.compile(datapack, ctx);
                }

                last.map_or(LowExpression::Unit, |last| {
                    last.compile(datapack, ctx).unwrap_or(LowExpression::Unit)
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

                LowExpression::Data(Box::new((output_target, output_path)))
            }
            Self::Loop(expression) => expression.kind.resolve(datapack, ctx),
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
            Self::Block(statements) => {
                for statement in statements {
                    statement.compile_as_statement(datapack, ctx);
                }
            }
            Self::If(condition, body, else_body) => {
                compile_if_internal(datapack, ctx, *condition, *body, else_body, None);
            }
            Self::Loop(expression) => expression.kind.compile_as_statement(datapack, ctx),
            Self::Underscore => {
                #[cfg(debug_assertions)]
                unreachable!();
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
            | Self::Variable(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub data_type: DataType,
}
