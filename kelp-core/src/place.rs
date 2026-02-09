use minecraft_command_types::{
    command::{
        Command, PlayerScore,
        data::DataTarget,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
};
use ordered_float::NotNan;
use parser_rs::parser_range::ParserRange;

use crate::{
    command::PlayerScoreExt,
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{
        Expression, ExpressionKind,
        constant::{ConstantExpression, ConstantExpressionKind},
        literal::{LiteralExpression, LiteralExpressionKind},
    },
    operator::ArithmeticOperator,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionIterExt,
};

fn constant_augmented_assign(
    target: LiteralExpressionKind,
    operator: ArithmeticOperator,
    value: LiteralExpressionKind,
) -> Option<LiteralExpressionKind> {
    Some(match (target, value) {
        (LiteralExpressionKind::Byte(left), LiteralExpressionKind::Byte(right)) => {
            LiteralExpressionKind::Byte(match operator {
                ArithmeticOperator::Add => left.wrapping_add(right),
                ArithmeticOperator::Subtract => left.wrapping_sub(right),
                ArithmeticOperator::Multiply => left.wrapping_mul(right),
                ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                ArithmeticOperator::Modulo => left % right,
                ArithmeticOperator::And => left & right,
                ArithmeticOperator::Or => left | right,
                ArithmeticOperator::LeftShift => left << right,
                ArithmeticOperator::RightShift => left >> right,
                ArithmeticOperator::Swap => unreachable!(),
            })
        }
        (LiteralExpressionKind::Short(left), LiteralExpressionKind::Short(right)) => {
            LiteralExpressionKind::Short(match operator {
                ArithmeticOperator::Add => left.wrapping_add(right),
                ArithmeticOperator::Subtract => left.wrapping_sub(right),
                ArithmeticOperator::Multiply => left.wrapping_mul(right),
                ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                ArithmeticOperator::Modulo => left % right,
                ArithmeticOperator::And => left & right,
                ArithmeticOperator::Or => left | right,
                ArithmeticOperator::LeftShift => left << right,
                ArithmeticOperator::RightShift => left >> right,
                ArithmeticOperator::Swap => unreachable!(),
            })
        }
        (LiteralExpressionKind::Integer(left), LiteralExpressionKind::Integer(right)) => {
            LiteralExpressionKind::Integer(match operator {
                ArithmeticOperator::Add => left.wrapping_add(right),
                ArithmeticOperator::Subtract => left.wrapping_sub(right),
                ArithmeticOperator::Multiply => left.wrapping_mul(right),
                ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                ArithmeticOperator::Modulo => left % right,
                ArithmeticOperator::And => left & right,
                ArithmeticOperator::Or => left | right,
                ArithmeticOperator::LeftShift => left << right,
                ArithmeticOperator::RightShift => left >> right,
                ArithmeticOperator::Swap => unreachable!(),
            })
        }
        (LiteralExpressionKind::Long(left), LiteralExpressionKind::Long(right)) => {
            LiteralExpressionKind::Long(match operator {
                ArithmeticOperator::Add => left.wrapping_add(right),
                ArithmeticOperator::Subtract => left.wrapping_sub(right),
                ArithmeticOperator::Multiply => left.wrapping_mul(right),
                ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                ArithmeticOperator::Modulo => left % right,
                ArithmeticOperator::And => left & right,
                ArithmeticOperator::Or => left | right,
                ArithmeticOperator::LeftShift => left << right,
                ArithmeticOperator::RightShift => left >> right,
                ArithmeticOperator::Swap => unreachable!(),
            })
        }
        (LiteralExpressionKind::Float(left), LiteralExpressionKind::Float(right)) => {
            LiteralExpressionKind::Float(match operator {
                ArithmeticOperator::Add => left + right,
                ArithmeticOperator::Subtract => left - right,
                ArithmeticOperator::Multiply => left * right,
                ArithmeticOperator::FloorDivide => left / right,
                ArithmeticOperator::Modulo => left % right,
                _ => unreachable!(),
            })
        }
        (LiteralExpressionKind::Double(left), LiteralExpressionKind::Double(right)) => {
            LiteralExpressionKind::Double(match operator {
                ArithmeticOperator::Add => left + right,
                ArithmeticOperator::Subtract => left - right,
                ArithmeticOperator::Multiply => left * right,
                ArithmeticOperator::FloorDivide => left / right,
                ArithmeticOperator::Modulo => left % right,
                _ => unreachable!(),
            })
        }
        _ => unreachable!(),
    })
}

#[derive(Debug)]
pub enum Place {
    Score(PlayerScore),
    Data(DataTarget, NbtPath),
    Variable(String),
    Tuple(Vec<Place>),
    Underscore,
}

impl Place {
    pub fn assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ConstantExpression,
    ) {
        match self {
            Place::Score(score) => {
                value.kind.assign_to_score(datapack, ctx, score);
            }
            Place::Data(target, path) => {
                value.kind.assign_to_data(datapack, ctx, target, path);
            }
            Place::Variable(name) => {
                datapack.assign_variable(&name, value);
            }
            Place::Underscore => {}
            Place::Tuple(places) => {
                if let ConstantExpressionKind::Tuple(values) = value.kind {
                    if places.len() != values.len() {
                        return;
                    }

                    let safe_values: Vec<ConstantExpression> = values
                        .into_iter()
                        .map(|v| match v.kind {
                            ConstantExpressionKind::PlayerScore(_) => {
                                let unique = v.kind.as_score(datapack, ctx, true);
                                ConstantExpressionKind::PlayerScore(unique)
                                    .into_dummy_constant_expression()
                            }
                            ConstantExpressionKind::Data(_, _) => {
                                let (t, p) = v.kind.as_data_force(datapack, ctx);
                                ConstantExpressionKind::Data(t, p).into_dummy_constant_expression()
                            }
                            _ => v,
                        })
                        .collect();

                    for (place, val) in places.into_iter().zip(safe_values) {
                        place.assign(datapack, ctx, val);
                    }
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    ) {
        match self {
            Place::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Place::Data(target, path) => {
                let unique_score = datapack.get_unique_player_score();

                ConstantExpressionKind::Data(target.clone(), path.clone()).assign_to_score(
                    datapack,
                    ctx,
                    unique_score.clone(),
                );

                unique_score
                    .clone()
                    .assign_augmented(datapack, ctx, operator, value);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target,
                            path,
                            NumericSNBTType::Integer,
                            NotNan::new(1.0).unwrap(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Scoreboard(
                                ScoreboardCommand::Players(PlayersScoreboardCommand::Get(
                                    unique_score,
                                )),
                            )))),
                        ),
                    )),
                );
            }
            Place::Variable(name) => {
                let (_, variable_value) = datapack.get_variable(&name).unwrap().clone();

                if let ConstantExpressionKind::Literal(target) = variable_value.kind
                    && let ConstantExpressionKind::Literal(value) = value
                    && let Some(result) =
                        constant_augmented_assign(target.kind, operator, value.kind)
                {
                    let variable_value_mut = &mut datapack.get_variable_mut(&name).unwrap().kind;

                    *variable_value_mut = ConstantExpressionKind::Literal(LiteralExpression {
                        span: ParserRange { start: 0, end: 0 },
                        kind: result,
                    });
                }
            }
            Place::Tuple(_) | Place::Underscore => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceType {
    Score,
    Data,
    Tuple(Vec<PlaceType>),
    Variable(DataTypeKind),
    Underscore,
}

impl PlaceType {
    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value: Expression,
        value_type: &DataTypeKind,
    ) -> Option<()> {
        match self {
            PlaceType::Score => {
                if !value_type.can_be_assigned_to_score() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToScore(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceType::Data => {
                if !value_type.can_be_assigned_to_data() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToData(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceType::Tuple(place_types) => {
                if let ExpressionKind::Tuple(expressions) = value.kind {
                    if expressions.len() != place_types.len() {
                        unreachable!();
                    }

                    return place_types
                        .into_iter()
                        .zip(expressions)
                        .map(|(place_type, value)| {
                            let value_type = value.kind.infer_data_type(ctx).unwrap();

                            place_type.perform_assignment_semantic_analysis(ctx, value, &value_type)
                        })
                        .all_some();
                } else {
                    unreachable!()
                }
            }
            PlaceType::Variable(data_type) => {
                if !data_type.equals(value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type,
                                actual: value_type.clone(),
                            },
                        ),
                    });
                }
            }
            PlaceType::Underscore => {}
        }

        Some(())
    }
}
