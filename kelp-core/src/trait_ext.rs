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
    compile_context::CompileContext, datapack::Datapack, player_score::GeneratedPlayerScore,
};

pub trait CollectOptionAllIterExt<T>: Iterator<Item = Option<T>> + Sized {
    fn collect_option_all<C>(self) -> Option<C>
    where
        C: FromIterator<T>;

    fn run_all_succeeded(self) -> Option<()>;
}

impl<T, I> CollectOptionAllIterExt<T> for I
where
    I: Iterator<Item = Option<T>>,
{
    fn collect_option_all<C>(self) -> Option<C>
    where
        C: FromIterator<T>,
    {
        let mut failed = false;

        let collection: C = self
            .filter_map(|item| {
                item.map_or_else(
                    || {
                        failed = true;

                        None
                    },
                    |v| Some(v),
                )
            })
            .collect();

        if failed { None } else { Some(collection) }
    }

    fn run_all_succeeded(self) -> Option<()> {
        let mut failed = false;

        for item in self {
            if item.is_none() {
                failed = true;
            }
        }

        (!failed).then_some(())
    }
}

pub trait OptionBoolIterExt {
    fn run_all_succeeded_true(self) -> Option<bool>;
}

impl<I> OptionBoolIterExt for I
where
    I: Iterator<Item = Option<bool>>,
{
    fn run_all_succeeded_true(self) -> Option<bool> {
        let mut failed = false;
        let mut result = false;

        for item in self {
            if let Some(value) = item {
                result |= !value;
            } else {
                failed = true;
            }
        }

        (!failed).then_some(!result)
    }
}

pub trait FilterOptionIteratorExt: Iterator {
    fn filter_all<F>(self, predicate: F) -> Option<bool>
    where
        F: Fn(&Self::Item) -> Option<bool>;

    fn filter_any<F>(self, predicate: F) -> Option<bool>
    where
        F: Fn(&Self::Item) -> Option<bool>;
}

impl<I> FilterOptionIteratorExt for I
where
    I: Iterator,
{
    fn filter_all<F>(self, predicate: F) -> Option<bool>
    where
        F: Fn(&Self::Item) -> Option<bool>,
    {
        for item in self {
            match predicate(&item) {
                Some(true) => {}
                Some(false) => return Some(false),
                None => return None,
            }
        }

        Some(true)
    }

    fn filter_any<F>(self, predicate: F) -> Option<bool>
    where
        F: Fn(&Self::Item) -> Option<bool>,
    {
        let mut any = false;

        for item in self {
            match predicate(&item) {
                Some(value) => any |= value,
                None => return None,
            }
        }

        Some(any)
    }
}

pub fn compile_bitwise_and_score(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: GeneratedPlayerScore,
    source: GeneratedPlayerScore,
) {
    let temp_target = datapack.get_unique_score();
    target
        .clone()
        .assign_to_score(datapack, ctx, temp_target.clone());

    let temp_source = datapack.get_unique_score();
    source.assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.score.clone(),
            0,
        ))),
    );

    let sign_bit_val = -2_147_483_648;
    let sign_bit_const = datapack.get_constant_score(sign_bit_val);

    ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                temp_target.score.clone(),
                ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                Some(Box::new(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        temp_source.score.clone(),
                        ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            result
                                .clone()
                                .operation(ScoreOperationOperator::Add, sign_bit_const.clone()),
                        )))),
                    ),
                ))),
            ),
        )),
    );

    for score in [&temp_target, &temp_source] {
        ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::If(
                false,
                ExecuteIfSubcommand::Score(
                    score.score.clone(),
                    ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                    Some(Box::new(ExecuteSubcommand::Run(Box::new(
                        score
                            .clone()
                            .operation(ScoreOperationOperator::Subtract, sign_bit_const.clone()),
                    )))),
                ),
            )),
        );
    }

    let power_of_2 = datapack.get_unique_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            power_of_2.score.clone(),
            1_073_741_824,
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
                        temp_target.score.clone(),
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

            for score in [&temp_target, &temp_source] {
                loop_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(
                        false,
                        ExecuteIfSubcommand::Score(
                            score.score.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.score.clone(),
                            ),
                            Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                score.clone().operation(
                                    ScoreOperationOperator::Subtract,
                                    power_of_2.clone(),
                                ),
                            )))),
                        ),
                    )),
                );
            }

            let constant_two = datapack.get_constant_score(2);
            loop_ctx.add_command(
                datapack,
                power_of_2
                    .clone()
                    .operation(ScoreOperationOperator::Divide, constant_two),
            );
        },
    );

    result.assign_to_score(datapack, ctx, target);
}

pub fn compile_bitwise_or_score(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: GeneratedPlayerScore,
    source: GeneratedPlayerScore,
) {
    let temp_target = datapack.get_unique_score();
    target
        .clone()
        .assign_to_score(datapack, ctx, temp_target.clone());

    let temp_source = datapack.get_unique_score();
    source.assign_to_score(datapack, ctx, temp_source.clone());

    let result = datapack.get_unique_score();
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
            result.score.clone(),
            0,
        ))),
    );

    let sign_bit_val = -2_147_483_648;
    let sign_bit_const = datapack.get_constant_score(sign_bit_val);

    ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                temp_target.score.clone(),
                ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                    result
                        .clone()
                        .operation(ScoreOperationOperator::Add, sign_bit_const.clone()),
                )))),
            ),
        )),
    );
    ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            true,
            ExecuteIfSubcommand::Score(
                temp_target.score.clone(),
                ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                Some(Box::new(ExecuteSubcommand::If(
                    false,
                    ExecuteIfSubcommand::Score(
                        temp_source.score.clone(),
                        ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                        Some(Box::new(ExecuteSubcommand::Run(Box::new(
                            result
                                .clone()
                                .operation(ScoreOperationOperator::Add, sign_bit_const.clone()),
                        )))),
                    ),
                ))),
            ),
        )),
    );

    for score in [&temp_target, &temp_source] {
        ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::If(
                false,
                ExecuteIfSubcommand::Score(
                    score.score.clone(),
                    ScoreComparison::Range(IntegerRange::new(None, Some(-1))),
                    Some(Box::new(ExecuteSubcommand::Run(Box::new(
                        score
                            .clone()
                            .operation(ScoreOperationOperator::Subtract, sign_bit_const.clone()),
                    )))),
                ),
            )),
        );
    }

    let power_of_2 = datapack.get_unique_score();
    ctx.add_command(
        datapack,
        power_of_2.clone().create_set_command(1_073_741_824),
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
                        temp_target.score.clone(),
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
                Command::Execute(ExecuteSubcommand::If(
                    true,
                    ExecuteIfSubcommand::Score(
                        temp_target.score.clone(),
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

            for score in [&temp_target, &temp_source] {
                loop_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(
                        false,
                        ExecuteIfSubcommand::Score(
                            score.score.clone(),
                            ScoreComparison::Score(
                                ScoreComparisonOperator::GreaterThanOrEqualTo,
                                power_of_2.score.clone(),
                            ),
                            Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                score.clone().operation(
                                    ScoreOperationOperator::Subtract,
                                    power_of_2.clone(),
                                ),
                            )))),
                        ),
                    )),
                );
            }

            let constant_two = datapack.get_constant_score(2);
            loop_ctx.add_command(
                datapack,
                power_of_2
                    .clone()
                    .operation(ScoreOperationOperator::Divide, constant_two),
            );
        },
    );

    result.assign_to_score(datapack, ctx, target);
}

pub fn compile_shift_operation_score(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: GeneratedPlayerScore,
    amount: GeneratedPlayerScore,
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

    loop_ctx.add_command(datapack, target.operation(operator, constant_two));
    loop_ctx.add_command(datapack, amount.clone().create_remove_command(1));

    loop_ctx.add_command(
        datapack,
        Command::Execute(ExecuteSubcommand::If(
            false,
            ExecuteIfSubcommand::Score(
                amount.score,
                ScoreComparison::Range(IntegerRange::new(Some(1), None)),
                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                    Command::Function(loop_function_location, None),
                )))),
            ),
        )),
    );

    datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_ctx);
}
