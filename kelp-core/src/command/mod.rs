use std::collections::BTreeMap;

use crate::compile_context::CompileContext;
use crate::datapack::HighDatapack;
use crate::expression::{ArithmeticOperator, ConstantExpressionKind};
use minecraft_command_types::command::data::DataTarget;
use minecraft_command_types::command::enums::numeric_snbt_type::NumericSNBTType;
use minecraft_command_types::command::enums::score_operation_operator::ScoreOperationOperator;
use minecraft_command_types::command::enums::store_type::StoreType;
use minecraft_command_types::command::execute::{
    ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
    ScoreComparisonOperator,
};
use minecraft_command_types::command::scoreboard::{PlayersScoreboardCommand, ScoreboardCommand};
use minecraft_command_types::command::{Command, PlayerScore};
use minecraft_command_types::nbt_path::NbtPath;
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::{SNBT, SNBTString};
use ordered_float::NotNan;

pub fn compile_bitwise_and_score(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &PlayerScore,
    source: &PlayerScore,
) {
    let temp_source = datapack.get_unique_player_score();
    source
        .clone()
        .assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.clone(),
            0,
        ))),
    );

    let power_of_2 = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            power_of_2.clone(),
            1073741824,
        ))),
    );

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.clone(),
                    ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                    None,
                ),
            )
        },
        |datapack, loop_ctx| {
            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        target.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Score(
                                temp_source.clone(),
                                ScoreComparison::Score(
                                    ScoreComparisonOperator::GreaterThanOrEqualTo,
                                    power_of_2.clone(),
                                ),
                                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                    result
                                        .clone()
                                        .operation(ScoreOperationOperator::Add, power_of_2.clone()),
                                )))),
                            ),
                        ))),
                    ),
                )),
            );

            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        target.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            target
                                .clone()
                                .operation(ScoreOperationOperator::Subtract, power_of_2.clone()),
                        )))),
                    ),
                )),
            );

            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        temp_source.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            temp_source
                                .clone()
                                .operation(ScoreOperationOperator::Subtract, power_of_2.clone()),
                        )))),
                    ),
                )),
            );

            let constant_two = datapack.get_constant_score(2);
            loop_ctx.add_command(
                datapack,
                power_of_2
                    .clone()
                    .operation(ScoreOperationOperator::Divide, constant_two),
            );
        },
    );

    result.assign_to_score(datapack, ctx, target.clone());
}

pub fn compile_bitwise_or_score(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &PlayerScore,
    source: &PlayerScore,
) {
    let temp_source = datapack.get_unique_player_score();
    source
        .clone()
        .assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.clone(),
            0,
        ))),
    );

    let power_of_2 = datapack.get_unique_player_score();
    ctx.add_command(datapack, power_of_2.clone().set(1073741824));

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.clone(),
                    ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                    None,
                ),
            )
        },
        |datapack, loop_ctx| {
            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        target.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            result
                                .clone()
                                .operation(ScoreOperationOperator::Add, power_of_2.clone()),
                        )))),
                    ),
                )),
            );
            loop_ctx.add_command(
                datapack,
                Command::Execute(
                    ExecuteSubcommand::If(
                        true,
                        ExecuteIfSubcommand::Score(
                            target.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.clone(),
                            ),
                            None,
                        ),
                    )
                    .then(ExecuteSubcommand::If(
                        false,
                        ExecuteIfSubcommand::Score(
                            temp_source.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.clone(),
                            ),
                            Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                result
                                    .clone()
                                    .operation(ScoreOperationOperator::Add, power_of_2.clone()),
                            )))),
                        ),
                    )),
                ),
            );

            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        target.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            target
                                .clone()
                                .operation(ScoreOperationOperator::Subtract, power_of_2.clone()),
                        )))),
                    ),
                )),
            );

            loop_ctx.add_command(
                datapack,
                Command::Execute(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        temp_source.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            temp_source
                                .clone()
                                .operation(ScoreOperationOperator::Subtract, power_of_2.clone()),
                        )))),
                    ),
                )),
            );

            let constant_two = datapack.get_constant_score(2);
            loop_ctx.add_command(
                datapack,
                power_of_2
                    .clone()
                    .operation(ScoreOperationOperator::Divide, constant_two),
            );
        },
    );

    result.assign_to_score(datapack, ctx, target.clone());
}

pub fn compile_shift_operation_score(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &PlayerScore,
    amount: &PlayerScore,
    operator: ScoreOperationOperator,
) {
    let loop_function_paths = datapack.get_unique_function_paths();
    let current_namespace_name = datapack.current_namespace_name().to_string();
    let loop_function_location =
        ResourceLocation::new_namespace_paths(current_namespace_name, loop_function_paths.clone());

    ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                amount.clone(),
                ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                    Command::Function(loop_function_location.clone(), None),
                )))),
            ),
        )),
    );

    let mut loop_ctx = CompileContext::default();
    let constant_two = datapack.get_constant_score(2);

    loop_ctx.add_command(datapack, target.clone().operation(operator, constant_two));
    loop_ctx.add_command(datapack, amount.clone().remove(1));

    loop_ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                amount.clone(),
                ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                    Command::Function(loop_function_location, None),
                )))),
            ),
        )),
    );

    datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_ctx);
}

pub trait PlayerScoreExt {
    fn operation(self, operator: ScoreOperationOperator, other: PlayerScore) -> Command;

    fn add(self, value: i32) -> Command;

    fn remove(self, value: i32) -> Command;

    fn set(self, value: i32) -> Command;

    fn to_text_component(self) -> SNBT;

    fn assign_to_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    );

    fn operate_on_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
        operator: ArithmeticOperator,
    );

    fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    );

    fn assign_augmented(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    );
}

impl PlayerScoreExt for PlayerScore {
    fn operation(self, operator: ScoreOperationOperator, other: PlayerScore) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(
            PlayersScoreboardCommand::Operation(self, operator, other),
        ))
    }

    fn add(self, value: i32) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Add(
            self, value,
        )))
    }

    fn remove(self, value: i32) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(
            PlayersScoreboardCommand::Remove(self, value),
        ))
    }

    fn set(self, value: i32) -> Command {
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            self, value,
        )))
    }

    fn to_text_component(self) -> SNBT {
        let mut text_component = BTreeMap::new();
        let mut score = BTreeMap::new();
        score.insert(
            SNBTString(false, "name".to_string()),
            SNBT::string(self.selector),
        );
        score.insert(
            SNBTString(false, "objective".to_string()),
            SNBT::string(self.objective),
        );
        text_component.insert(
            SNBTString(false, "score".to_string()),
            SNBT::Compound(score),
        );
        SNBT::Compound(text_component)
    }

    #[inline]
    fn assign_to_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    ) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(
                PlayersScoreboardCommand::Operation(target, ScoreOperationOperator::Set, self),
            )),
        );
    }

    fn operate_on_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
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
                            target.clone(),
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
                            self.clone(),
                        ),
                    )),
                );
            }
        }
    }

    fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    ) {
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
                        ScoreboardCommand::Players(PlayersScoreboardCommand::Get(self)),
                    )))),
                ),
            )),
        );
    }

    fn assign_augmented(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    ) {
        match operator {
            ArithmeticOperator::Add => {
                if let Some(constant) = value.try_as_i32(true) {
                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Add(self, constant),
                        )),
                    );
                } else {
                    let right_score = value.as_score(datapack, ctx, true);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                self,
                                ScoreOperationOperator::Add,
                                right_score,
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
                            PlayersScoreboardCommand::Remove(self, constant),
                        )),
                    );
                } else {
                    let right_score = value.as_score(datapack, ctx, true);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                self,
                                ScoreOperationOperator::Subtract,
                                right_score,
                            ),
                        )),
                    );
                }
            }
            ArithmeticOperator::Multiply => {
                let right_score = value.as_score(datapack, ctx, true);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self,
                            ScoreOperationOperator::Multiply,
                            right_score,
                        ),
                    )),
                );
            }
            ArithmeticOperator::FloorDivide => {
                let right_score = value.as_score(datapack, ctx, true);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self,
                            ScoreOperationOperator::Divide,
                            right_score,
                        ),
                    )),
                );
            }
            ArithmeticOperator::Modulo => {
                let right_score = value.as_score(datapack, ctx, true);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self,
                            ScoreOperationOperator::Modulo,
                            right_score,
                        ),
                    )),
                );
            }
            ArithmeticOperator::And => {
                let right_score = value.as_score(datapack, ctx, true);

                compile_bitwise_and_score(datapack, ctx, &self, &right_score);
            }
            ArithmeticOperator::Or => {
                let right_score = value.as_score(datapack, ctx, true);

                compile_bitwise_or_score(datapack, ctx, &self, &right_score);
            }
            ArithmeticOperator::LeftShift => {
                let right_score = value.as_score(datapack, ctx, true);

                compile_shift_operation_score(
                    datapack,
                    ctx,
                    &self,
                    &right_score,
                    ScoreOperationOperator::Multiply,
                );
            }
            ArithmeticOperator::RightShift => {
                let right_score = value.as_score(datapack, ctx, true);

                compile_shift_operation_score(
                    datapack,
                    ctx,
                    &self,
                    &right_score,
                    ScoreOperationOperator::Divide,
                );
            }
            ArithmeticOperator::Swap => {
                let right_score = value.as_score(datapack, ctx, true);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            self,
                            ScoreOperationOperator::Swap,
                            right_score,
                        ),
                    )),
                );
            }
        }
    }
}
