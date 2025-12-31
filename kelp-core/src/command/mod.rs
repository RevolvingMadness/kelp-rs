use std::collections::BTreeMap;

use crate::command::context::CompileContext;
use crate::command::data::HighDataCommand;
use crate::command::execute::HighExecuteSubcommand;
use crate::command::function::HighFunctionCommandArguments;
use crate::command::r#return::HighReturnCommand;
use crate::command::scoreboard::HighScoreboardCommand;
use crate::datapack::HighDatapack;
use crate::entity_selector::HighEntitySelector;
use crate::expression::{ArithmeticOperator, Expression};
use minecraft_command_types::command::data::DataTarget;
use minecraft_command_types::command::enums::numeric_snbt_type::NumericSNBTType;
use minecraft_command_types::command::enums::score_operation_operator::ScoreOperationOperator;
use minecraft_command_types::command::enums::store_type::StoreType;
use minecraft_command_types::command::execute::{
    ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
    ScoreComparisonOperator,
};
use minecraft_command_types::command::r#return::ReturnCommand;
use minecraft_command_types::command::scoreboard::{PlayersScoreboardCommand, ScoreboardCommand};
use minecraft_command_types::command::{Command, PlayerScore};
use minecraft_command_types::coordinate::Coordinates;
use minecraft_command_types::nbt_path::NbtPath;
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::snbt::{SNBT, SNBTString};
use minecraft_command_types::{
    command::enums::difficulty::Difficulty, resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

pub mod context;
pub mod data;
pub mod execute;
pub mod function;
pub mod item_source;
pub mod r#return;
pub mod scoreboard;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighPlayerScore {
    pub is_generated: bool,
    pub selector: HighEntitySelector,
    pub objective: String,
}

impl HighPlayerScore {
    pub fn new(selector: HighEntitySelector, objective: String) -> Self {
        Self {
            is_generated: false,
            selector,
            objective,
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> PlayerScore {
        PlayerScore::new(self.selector.compile(datapack, ctx), self.objective)
    }
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
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
        operator: ArithmeticOperator,
    );

    fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
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
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
        operator: ArithmeticOperator,
    ) {
        match operator {
            ArithmeticOperator::And => {
                compile_bitwise_and_score(datapack, ctx, &target, &self);
            }
            ArithmeticOperator::Or => {
                compile_bitwise_or_score(datapack, ctx, &target, &self);
            }
            ArithmeticOperator::LeftShift => {
                compile_shift_operation_score(
                    datapack,
                    ctx,
                    &target,
                    &self,
                    ScoreOperationOperator::Multiply,
                );
            }
            ArithmeticOperator::RightShift => {
                compile_shift_operation_score(
                    datapack,
                    ctx,
                    &target,
                    &self,
                    ScoreOperationOperator::Divide,
                );
            }
            _ => {
                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            target,
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
                            self,
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
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighCommand {
    Regular(Command),
    Data(HighDataCommand),
    Difficulty(Difficulty),
    Enchant(HighEntitySelector, ResourceLocation, Option<i32>),
    Execute(HighExecuteSubcommand),
    Function(ResourceLocation, Option<HighFunctionCommandArguments>),
    Tellraw(HighEntitySelector, Expression),
    Return(HighReturnCommand),
    Scoreboard(HighScoreboardCommand),
    Summon(ResourceLocation, Option<Coordinates>, Option<Expression>),
}

impl HighCommand {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Command> {
        match self {
            HighCommand::Regular(command) => Some(command),
            HighCommand::Data(data_command) => data_command.compile(datapack, ctx),
            HighCommand::Difficulty(difficulty_command) => {
                Some(Command::Difficulty(difficulty_command))
            }
            HighCommand::Enchant(selector, location, level) => Some(Command::Enchant(
                selector.compile(datapack, ctx),
                location,
                level,
            )),
            HighCommand::Execute(execute_subcommand) => execute_subcommand
                .compile(datapack, ctx)
                .map(Command::Execute),
            HighCommand::Function(id, arguments) => {
                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(datapack, ctx));
                Some(Command::Function(id, compiled_arguments))
            }
            HighCommand::Tellraw(selector, expression) => {
                let expression = expression.resolve(datapack, ctx);

                Some(Command::Tellraw(
                    selector.compile(datapack, ctx),
                    expression.kind.as_text_component(datapack, ctx),
                ))
            }
            HighCommand::Return(command) => match command {
                HighReturnCommand::Fail | HighReturnCommand::Value(0) => {
                    Some(Command::Return(ReturnCommand::Fail))
                }
                HighReturnCommand::Value(value) => {
                    Some(Command::Return(ReturnCommand::Value(value)))
                }
                HighReturnCommand::Run(command) => {
                    command.compile(datapack, ctx).map(|compiled_command| {
                        Command::Return(ReturnCommand::Run(Box::new(compiled_command)))
                    })
                }
            },
            HighCommand::Scoreboard(command) => {
                Some(Command::Scoreboard(command.compile(datapack, ctx)))
            }
            HighCommand::Summon(entity, position, nbt) => Some(Command::Summon(
                entity,
                position,
                nbt.map(|nbt| nbt.resolve(datapack, ctx).kind.as_snbt_macros(ctx)),
            )),
        }
    }
}

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
