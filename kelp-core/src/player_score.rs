use minecraft_command_types::{
    command::{
        Command, PlayerScore, ScoreValue,
        enums::{
            numeric_snbt_type::NumericSNBTType, score_operation_operator::ScoreOperationOperator,
        },
        execute::{ExecuteIfSubcommand, ScoreComparison},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::SNBTCompound,
    range::IntegerRange,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedData,
    datapack::Datapack,
    low::expression::resolved::ResolvedExpression,
    operator::ArithmeticOperator,
    trait_ext::{
        compile_bitwise_and_score, compile_bitwise_or_score, compile_shift_operation_score,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GeneratedPlayerScore {
    pub is_generated: bool,
    pub score: PlayerScore,
}

impl GeneratedPlayerScore {
    #[must_use]
    pub fn as_unique_score(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Self {
        let unique_score = datapack.get_unique_score();

        unique_score.clone().set_from(datapack, ctx, self);

        unique_score
    }

    #[must_use]
    pub fn to_execute_condition(self) -> (bool, ExecuteIfSubcommand) {
        (
            true,
            ExecuteIfSubcommand::Score(
                self.score,
                ScoreComparison::Range(IntegerRange::new_single(0)),
                None,
            ),
        )
    }

    #[must_use]
    pub fn to_text_component(self) -> SNBT {
        let mut text_component = SNBTCompound::new();
        let mut score = SNBTCompound::new();
        score.insert(
            SNBTString(false, "name".to_string()),
            SNBT::macroable_string(self.score.selector.to_string()),
        );
        score.insert(
            SNBTString(false, "objective".to_string()),
            SNBT::macroable_string(self.score.objective),
        );
        text_component.insert(
            SNBTString(false, "score".to_string()),
            SNBT::macroable_compound(score),
        );
        SNBT::compound(text_component)
    }

    #[inline]
    pub fn set_from(self, datapack: &mut Datapack, ctx: &mut CompileContext, source: Self) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(
                PlayersScoreboardCommand::Operation(
                    self.score,
                    ScoreOperationOperator::Set,
                    source.score,
                ),
            )),
        );
    }

    pub fn assign_to_score_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: Self,
        scale: NotNan<f32>,
    ) {
        let unique_score = datapack.get_unique_score();

        unique_score.clone().set_from(datapack, ctx, self);

        let scale_score = datapack.get_constant_score(scale.into_inner() as ScoreValue);

        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(
                PlayersScoreboardCommand::Operation(
                    unique_score.score.clone(),
                    ScoreOperationOperator::Multiply,
                    scale_score.score,
                ),
            )),
        );

        target.set_from(datapack, ctx, unique_score);
    }

    pub fn operate_on_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: Self,
        operator: ArithmeticOperator,
    ) {
        match operator {
            ArithmeticOperator::And => {
                compile_bitwise_and_score(datapack, ctx, target, self);
            }
            ArithmeticOperator::Or => {
                compile_bitwise_or_score(datapack, ctx, target, self);
            }
            ArithmeticOperator::LeftShift => {
                compile_shift_operation_score(
                    datapack,
                    ctx,
                    target,
                    self,
                    ScoreOperationOperator::Multiply,
                );
            }
            ArithmeticOperator::RightShift => {
                compile_shift_operation_score(
                    datapack,
                    ctx,
                    target,
                    self,
                    ScoreOperationOperator::Divide,
                );
            }
            _ => {
                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            target.score,
                            operator.try_into().unwrap(),
                            self.score,
                        ),
                    )),
                );
            }
        }
    }

    #[inline]
    pub fn assign_to_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data: GeneratedData,
    ) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Get(
                self.score,
            )))
            .run()
            .store_result_data(
                data.target.target,
                data.path,
                NumericSNBTType::Integer,
                NotNan::new(1.0).unwrap(),
            ),
        );
    }

    #[inline]
    pub fn assign_to_data_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data: GeneratedData,
        scale: NotNan<f32>,
    ) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Get(
                self.score,
            )))
            .run()
            .store_result_data(
                data.target.target,
                data.path,
                NumericSNBTType::Float,
                scale,
            ),
        );
    }

    pub fn assign_augmented(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ResolvedExpression,
    ) {
        match operator {
            ArithmeticOperator::Add => {
                if let Some(constant) = value.try_as_i32(true) {
                    ctx.add_command(datapack, self.score.add_value(constant));
                } else {
                    let right_score = value.as_score(datapack, ctx, false);

                    ctx.add_command(datapack, self.score.add(right_score.score));
                }
            }
            ArithmeticOperator::Subtract => {
                if let Some(constant) = value.try_as_i32(true) {
                    ctx.add_command(datapack, self.score.remove(constant));
                } else {
                    let right_score = value.as_score(datapack, ctx, false);

                    ctx.add_command(datapack, self.score.subtract(right_score.score));
                }
            }
            ArithmeticOperator::Multiply => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(datapack, self.score.multiply(right_score.score));
            }
            ArithmeticOperator::FloorDivide => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(datapack, self.score.divide(right_score.score));
            }
            ArithmeticOperator::Modulo => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(datapack, self.score.modulo(right_score.score));
            }
            ArithmeticOperator::And => {
                let right_score = value.as_score(datapack, ctx, false);

                compile_bitwise_and_score(datapack, ctx, self, right_score);
            }
            ArithmeticOperator::Or => {
                let right_score = value.as_score(datapack, ctx, false);

                compile_bitwise_or_score(datapack, ctx, self, right_score);
            }
            ArithmeticOperator::LeftShift => {
                let right_score = value.as_score(datapack, ctx, false);

                compile_shift_operation_score(
                    datapack,
                    ctx,
                    self,
                    right_score,
                    ScoreOperationOperator::Multiply,
                );
            }
            ArithmeticOperator::RightShift => {
                let right_score = value.as_score(datapack, ctx, false);

                compile_shift_operation_score(
                    datapack,
                    ctx,
                    self,
                    right_score,
                    ScoreOperationOperator::Divide,
                );
            }
        }
    }
}
