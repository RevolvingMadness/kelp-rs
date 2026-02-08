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
    data_type::DataType,
    datapack::HighDatapack,
    expression::{ArithmeticOperator, ConstantExpression, ConstantExpressionKind},
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
};

fn constant_augmented_assign(
    target: ConstantExpressionKind,
    operator: ArithmeticOperator,
    value: ConstantExpressionKind,
) -> Option<ConstantExpressionKind> {
    Some(match (target, value) {
        (ConstantExpressionKind::Byte(left), ConstantExpressionKind::Byte(right)) => {
            ConstantExpressionKind::Byte(match operator {
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
        (ConstantExpressionKind::Short(left), ConstantExpressionKind::Short(right)) => {
            ConstantExpressionKind::Short(match operator {
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
        (ConstantExpressionKind::Integer(left), ConstantExpressionKind::Integer(right)) => {
            ConstantExpressionKind::Integer(match operator {
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
        (ConstantExpressionKind::Long(left), ConstantExpressionKind::Long(right)) => {
            ConstantExpressionKind::Long(match operator {
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
        (ConstantExpressionKind::Float(left), ConstantExpressionKind::Float(right)) => {
            ConstantExpressionKind::Float(match operator {
                ArithmeticOperator::Add => left + right,
                ArithmeticOperator::Subtract => left - right,
                ArithmeticOperator::Multiply => left * right,
                ArithmeticOperator::FloorDivide => left / right,
                ArithmeticOperator::Modulo => left % right,
                _ => unreachable!(),
            })
        }
        (ConstantExpressionKind::Double(left), ConstantExpressionKind::Double(right)) => {
            ConstantExpressionKind::Double(match operator {
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

pub enum Place {
    Score(PlayerScore),
    Data(DataTarget, NbtPath),
    Variable(String),
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

                let result = constant_augmented_assign(variable_value.kind, operator, value);

                if let Some(result) = result {
                    let variable_value_mut = &mut datapack.get_variable_mut(&name).unwrap().kind;

                    *variable_value_mut = result;
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceType {
    Score,
    Data,
    Variable(DataType),
}

impl PlaceType {
    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value_span: ParserRange,
        value_type: DataType,
    ) -> Option<()> {
        match self {
            PlaceType::Score => {
                if !value_type.can_be_assigned_to_score() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToScore(value_type),
                        ),
                    });
                }
            }
            PlaceType::Data => {
                if !value_type.can_be_assigned_to_data() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToData(value_type),
                        ),
                    });
                }
            }
            PlaceType::Variable(data_type) => {
                if !data_type.equals(&value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type,
                                actual: value_type,
                            },
                        ),
                    });
                }
            }
        }

        Some(())
    }
}
