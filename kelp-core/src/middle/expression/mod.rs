use std::collections::BTreeMap;

use minecraft_command_types::{
    command::{
        Command as LowCommand,
        enums::store_type::StoreType,
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
    },
    snbt::SNBTString,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::Expression as LowExpression,
    middle::{
        data::DataTarget,
        data_type::DataTypeKind,
        expression::command::{Command, execute::subcommand::r#if::ExecuteIfSubcommand},
        nbt_path::NbtPath,
        player_score::PlayerScore,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::Place,
    runtime_storage_type::RuntimeStorageType,
};

pub mod command;

#[derive(Debug, Clone, Hash, HasMacro)]
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
    Compound(BTreeMap<SNBTString, Expression>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),
    Condition(bool, Box<ExecuteIfSubcommand>),
    Command(Box<Command>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, SNBTString),
    AsCast(Box<Expression>, DataTypeKind),
    ToCast(Option<NotNan<f32>>, Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Variable(String),
    Struct(String, Vec<DataTypeKind>, BTreeMap<SNBTString, Expression>),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    #[must_use]
    pub const fn with(self, data_type: DataTypeKind) -> Expression {
        Expression {
            kind: self,
            data_type,
        }
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
                    let expression = expression.kind.resolve(datapack, ctx);

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

                Some(Place::Index(Box::new(target), Box::new(index)))
            }
            Self::FieldAccess(target, field) => {
                let target = target.kind.resolve(datapack, ctx);

                Some(Place::Field(Box::new(target), field.1))
            }
            Self::Tuple(tuple) => Some(Place::Tuple(
                tuple
                    .into_iter()
                    .map(|expression| expression.kind.as_place(datapack, ctx))
                    .collect::<Option<_>>()?,
            )),
            Self::Variable(name) => Some(Place::Variable(name)),
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
            | Self::Struct(_, _, _) => None,
        }
    }

    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowExpression {
        match self {
            Self::Unary(unary_operator, expression) => match unary_operator {
                UnaryOperator::Negate => {
                    let expression = expression.kind.resolve(datapack, ctx);

                    expression.negate(datapack, ctx)
                }
                UnaryOperator::Invert => {
                    let expression = expression.kind.resolve(datapack, ctx);

                    expression.invert()
                }
                UnaryOperator::Reference => expression.kind.resolve(datapack, ctx),
                UnaryOperator::Dereference => expression
                    .kind
                    .resolve(datapack, ctx)
                    .dereference(datapack, ctx)
                    .unwrap(),
            },
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
                let target_place = target.kind.as_place(datapack, ctx).unwrap();

                target_place.assign(datapack, ctx, value.kind);

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
                    .collect::<BTreeMap<_, _>>(),
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
                    LowCommand::Execute(ExecuteSubcommand::Store(
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
                let target = target.kind.resolve(datapack, ctx);

                target.access_field(&field.1).unwrap()
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
            Self::Struct(name, generics, fields) => LowExpression::Struct(
                name,
                generics,
                fields
                    .into_iter()
                    .map(|(key, field)| (key.1, field.kind.resolve(datapack, ctx)))
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
            Self::Variable(name) => datapack.get_variable(&name).unwrap().1,
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Assignment(target, value) => {
                let target_place = target.kind.as_place(datapack, ctx).unwrap();

                target_place.assign(datapack, ctx, value.kind);
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
                    LowCommand::Execute(ExecuteSubcommand::If(false, condition)),
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
            Self::Struct(_, _, field_expressions) => {
                for value in field_expressions.into_values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
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

#[derive(Debug, Clone, Hash, HasMacro)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub data_type: DataTypeKind,
}
