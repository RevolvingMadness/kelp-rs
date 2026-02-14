use minecraft_command_types::{
    command::{
        Command,
        enums::score_operation_operator::ScoreOperationOperator,
        execute::{
            ExecuteIfSubcommand, ExecuteSubcommand, ScoreComparison, ScoreComparisonOperator,
        },
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    range::IntegerRange,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::player_score::GeneratedPlayerScore,
};

pub trait OptionIterExt {
    fn all_some(self) -> Option<()>;
}

impl<I> OptionIterExt for I
where
    I: Iterator<Item = Option<()>>,
{
    fn all_some(self) -> Option<()> {
        let mut failed = false;

        for item in self {
            failed |= item.is_none();
        }

        (!failed).then_some(())
    }
}

pub trait OptionExt {
    fn panic_if_some(&self);
}

impl<T> OptionExt for Option<T> {
    fn panic_if_some(&self) {
        if self.is_some() {
            panic!("Option<T> must be None");
        }
    }
}

pub fn compile_bitwise_and_score(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &GeneratedPlayerScore,
    source: &GeneratedPlayerScore,
) {
    let temp_source = datapack.get_unique_player_score();
    source
        .clone()
        .assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.score.clone(),
            0,
        ))),
    );

    let power_of_2 = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            power_of_2.score.clone(),
            1073741824,
        ))),
    );

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.score.clone(),
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
                        target.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
                        ),
                        Some(Box::new(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Score(
                                temp_source.score.clone(),
                                ScoreComparison::Score(
                                    ScoreComparisonOperator::GreaterThanOrEqualTo,
                                    power_of_2.score.clone(),
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
                        target.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
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
                        temp_source.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
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
    target: &GeneratedPlayerScore,
    source: &GeneratedPlayerScore,
) {
    let temp_source = datapack.get_unique_player_score();
    source
        .clone()
        .assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_player_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.score.clone(),
            0,
        ))),
    );

    let power_of_2 = datapack.get_unique_player_score();
    ctx.add_command(datapack, power_of_2.clone().create_set_command(1073741824));

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.score.clone(),
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
                        target.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
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
                            target.score.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.score.clone(),
                            ),
                            None,
                        ),
                    )
                    .then(ExecuteSubcommand::If(
                        false,
                        ExecuteIfSubcommand::Score(
                            temp_source.score.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.score.clone(),
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
                        target.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
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
                        temp_source.score.clone(),
                        ScoreComparison::Score(
                            ScoreComparisonOperator::GreaterThanOrEqualTo,
                            power_of_2.score.clone(),
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
    target: &GeneratedPlayerScore,
    amount: &GeneratedPlayerScore,
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
                amount.score.clone(),
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
    loop_ctx.add_command(datapack, amount.clone().create_remove_command(1));

    loop_ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                amount.score.clone(),
                ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                    Command::Function(loop_function_location, None),
                )))),
            ),
        )),
    );

    datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_ctx);
}
