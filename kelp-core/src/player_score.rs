use std::collections::BTreeMap;

use minecraft_command_types::{
    command::{
        Command, PlayerScore,
        enums::{
            numeric_snbt_type::NumericSNBTType, score_operation_operator::ScoreOperationOperator,
            store_type::StoreType,
        },
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
        },
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
    range::IntegerRange,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::expression::Expression,
    operator::ArithmeticOperator,
    trait_ext::{
        compile_bitwise_and_score, compile_bitwise_or_score, compile_shift_operation_score,
    },
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct GeneratedPlayerScore {
    pub is_generated: bool,
    pub score: PlayerScore,
}

impl GeneratedPlayerScore {
    #[must_use]
    pub fn as_unique_score(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Self {
        let unique_score = datapack.get_unique_score();

        self.assign_to_score(datapack, ctx, unique_score.clone());

        unique_score
    }

    #[must_use]
    pub fn to_execute_condition(self, inverted: bool) -> (bool, ExecuteIfSubcommand) {
        (
            !inverted,
            ExecuteIfSubcommand::Score(
                self.score,
                ScoreComparison::Range(IntegerRange::new_single(0)),
                None,
            ),
        )
    }

    #[inline]
    #[must_use]
    pub fn operation(self, operator: ScoreOperationOperator, other: Self) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(
            PlayersScoreboardCommand::Operation(self.score, operator, other.score),
        ))
    }

    #[inline]
    #[must_use]
    pub fn create_remove_command(self, value: i32) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(
            PlayersScoreboardCommand::Remove(self.score, value),
        ))
    }

    #[inline]
    #[must_use]
    pub fn create_set_command(self, value: i32) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            self.score, value,
        )))
    }

    #[must_use]
    pub fn to_text_component(self) -> SNBT {
        let mut text_component = BTreeMap::new();
        let mut score = BTreeMap::new();
        score.insert(
            SNBTString(false, "name".to_string()),
            SNBT::string(self.score.selector.to_string()),
        );
        score.insert(
            SNBTString(false, "objective".to_string()),
            SNBT::string(self.score.objective),
        );
        text_component.insert(
            SNBTString(false, "score".to_string()),
            SNBT::Compound(score),
        );
        SNBT::Compound(text_component)
    }

    #[inline]
    pub fn assign_to_score(self, datapack: &mut Datapack, ctx: &mut CompileContext, target: Self) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(
                PlayersScoreboardCommand::Operation(
                    target.score,
                    ScoreOperationOperator::Set,
                    self.score,
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

        self.assign_to_score(datapack, ctx, unique_score.clone());

        let scale_score = datapack.get_constant_score(scale.into_inner() as i32);

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

        unique_score.assign_to_score(datapack, ctx, target);
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
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
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
        target: GeneratedDataTarget,
        path: NbtPath,
    ) {
        ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::Store(
                StoreType::Result,
                ExecuteStoreSubcommand::Data(
                    target.target,
                    path,
                    NumericSNBTType::Integer,
                    NotNan::new(1.0).unwrap(),
                    Box::new(ExecuteSubcommand::Run(Box::new(Command::Scoreboard(
                        ScoreboardCommand::Players(PlayersScoreboardCommand::Get(self.score)),
                    )))),
                ),
            )),
        );
    }

    #[inline]
    pub fn assign_to_data_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: GeneratedDataTarget,
        path: NbtPath,
        scale: NotNan<f32>,
    ) {
        ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::Store(
                StoreType::Result,
                ExecuteStoreSubcommand::Data(
                    target.target,
                    path,
                    NumericSNBTType::Float,
                    scale,
                    Box::new(ExecuteSubcommand::Run(Box::new(Command::Scoreboard(
                        ScoreboardCommand::Players(PlayersScoreboardCommand::Get(self.score)),
                    )))),
                ),
            )),
        );
    }

    pub fn assign_augmented(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: Expression,
    ) {
        match operator {
            ArithmeticOperator::Add => {
                if let Some(constant) = value.try_as_i32(true) {
                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Add(self.score, constant),
                        )),
                    );
                } else {
                    let right_score = value.as_score(datapack, ctx, false);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                self.score,
                                ScoreOperationOperator::Add,
                                right_score.score,
                            ),
                        )),
                    );
                }
            }
            ArithmeticOperator::Subtract => {
                if let Some(constant) = value.try_as_i32(true) {
                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Remove(self.score, constant),
                        )),
                    );
                } else {
                    let right_score = value.as_score(datapack, ctx, false);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                self.score,
                                ScoreOperationOperator::Subtract,
                                right_score.score,
                            ),
                        )),
                    );
                }
            }
            ArithmeticOperator::Multiply => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self.score,
                            ScoreOperationOperator::Multiply,
                            right_score.score,
                        ),
                    )),
                );
            }
            ArithmeticOperator::FloorDivide => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self.score,
                            ScoreOperationOperator::Divide,
                            right_score.score,
                        ),
                    )),
                );
            }
            ArithmeticOperator::Modulo => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self.score,
                            ScoreOperationOperator::Modulo,
                            right_score.score,
                        ),
                    )),
                );
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
            ArithmeticOperator::Swap => {
                let right_score = value.as_score(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self.score,
                            ScoreOperationOperator::Swap,
                            right_score.score,
                        ),
                    )),
                );
            }
        }
    }
}
