use minecraft_command_types::{
    command::{
        Command, ScoreValue,
        enums::score_operation_operator::ScoreOperationOperator,
        execute::{ExecuteIfSubcommand, ScoreComparison},
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
        let mut all = true;

        for item in self {
            match predicate(&item) {
                Some(true) => {}
                Some(false) => all = false,
                None => return None,
            }
        }

        Some(all)
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

    temp_target.clone().set_from(datapack, ctx, target.clone());

    let temp_source = datapack.get_unique_score();
    temp_source.clone().set_from(datapack, ctx, source);

    let result = datapack.get_unique_score();
    ctx.add_command(datapack, result.score.clone().set_value(0));

    let sign_bit_val = -2_147_483_648;
    let sign_bit_const = datapack.get_constant_score(sign_bit_val);

    ctx.add_command(
        datapack,
        result
            .clone()
            .score
            .add(sign_bit_const.score.clone())
            .run()
            .if_score_range(temp_source.score.clone(), None, Some(-1))
            .if_score_range(temp_target.score.clone(), None, Some(-1)),
    );

    for score in [&temp_target, &temp_source] {
        ctx.add_command(
            datapack,
            score
                .score
                .clone()
                .subtract(sign_bit_const.score.clone())
                .run()
                .if_score_range(score.score.clone(), None, Some(-1)),
        );
    }

    let power_of_2 = datapack.get_unique_score();
    ctx.add_command(datapack, power_of_2.score.clone().set_value(1_073_741_824));

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.score.clone(),
                    ScoreComparison::Range(IntegerRange::new_min(1)),
                    None,
                ),
            )
        },
        |datapack, loop_ctx| {
            loop_ctx.add_command(
                datapack,
                result
                    .score
                    .clone()
                    .add(power_of_2.score.clone())
                    .run()
                    .if_score_greater_than_or_equal_to(
                        temp_source.score.clone(),
                        power_of_2.score.clone(),
                    )
                    .if_score_greater_than_or_equal_to(
                        temp_target.score.clone(),
                        power_of_2.score.clone(),
                    ),
            );

            for score in [&temp_target, &temp_source] {
                loop_ctx.add_command(
                    datapack,
                    score
                        .score
                        .clone()
                        .subtract(power_of_2.score.clone())
                        .run()
                        .if_score_greater_than_or_equal_to(
                            score.score.clone(),
                            power_of_2.score.clone(),
                        ),
                );
            }

            let constant_two = datapack.get_constant_score(2);

            loop_ctx.add_command(
                datapack,
                power_of_2.score.clone().divide(constant_two.score),
            );
        },
    );

    target.set_from(datapack, ctx, result);
}

pub fn compile_bitwise_or_score(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: GeneratedPlayerScore,
    source: GeneratedPlayerScore,
) {
    let temp_target = datapack.get_unique_score();
    temp_target.clone().set_from(datapack, ctx, target.clone());

    let temp_source = datapack.get_unique_score();
    temp_source.clone().set_from(datapack, ctx, source);

    let result = datapack.get_unique_score();

    ctx.add_command(datapack, result.score.clone().set_value(0));

    let sign_bit_const = datapack.get_constant_score(ScoreValue::MIN);

    ctx.add_command(
        datapack,
        result
            .score
            .clone()
            .add(sign_bit_const.score.clone())
            .run()
            .if_score_range(temp_target.score.clone(), None, Some(-1)),
    );

    ctx.add_command(
        datapack,
        result
            .score
            .clone()
            .add(sign_bit_const.score.clone())
            .run()
            .if_score_range(temp_source.score.clone(), None, Some(-1))
            .unless_score_range(temp_target.score.clone(), None, Some(-1)),
    );

    for score in [&temp_target, &temp_source] {
        ctx.add_command(
            datapack,
            score
                .score
                .clone()
                .subtract(sign_bit_const.score.clone())
                .run()
                .if_score_range(score.score.clone(), None, Some(-1)),
        );
    }

    let power_of_2 = datapack.get_unique_score();
    ctx.add_command(datapack, power_of_2.score.clone().set_value(1_073_741_824));

    datapack.while_loop(
        ctx,
        |_, _| {
            (
                false,
                ExecuteIfSubcommand::Score(
                    power_of_2.score.clone(),
                    ScoreComparison::Range(IntegerRange::new_min(1)),
                    None,
                ),
            )
        },
        |datapack, loop_ctx| {
            loop_ctx.add_command(
                datapack,
                result
                    .score
                    .clone()
                    .add(power_of_2.score.clone())
                    .run()
                    .if_score_greater_than_or_equal_to(
                        temp_target.score.clone(),
                        power_of_2.score.clone(),
                    ),
            );

            loop_ctx.add_command(
                datapack,
                result
                    .score
                    .clone()
                    .add(power_of_2.score.clone())
                    .run()
                    .if_score_greater_than_or_equal_to(
                        temp_source.score.clone(),
                        power_of_2.score.clone(),
                    )
                    .unless_score_greater_than_or_equal_to(
                        temp_target.score.clone(),
                        power_of_2.score.clone(),
                    ),
            );

            for score in [&temp_target, &temp_source] {
                loop_ctx.add_command(
                    datapack,
                    score
                        .score
                        .clone()
                        .subtract(power_of_2.score.clone())
                        .run()
                        .if_score_greater_than_or_equal_to(
                            score.score.clone(),
                            power_of_2.score.clone(),
                        ),
                );
            }

            let constant_two = datapack.get_constant_score(2);

            loop_ctx.add_command(
                datapack,
                power_of_2.score.clone().divide(constant_two.score),
            );
        },
    );

    target.set_from(datapack, ctx, result);
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
        ResourceLocation::new_namespace_paths(&current_namespace_name, loop_function_paths.clone());

    ctx.add_command(
        datapack,
        Command::Function(loop_function_location.clone(), None)
            .run()
            .if_score_range(amount.score.clone(), Some(1), None),
    );

    let mut loop_ctx = CompileContext::default();
    let constant_two = datapack.get_constant_score(2);

    loop_ctx.add_command(
        datapack,
        target.score.operation(operator, constant_two.score),
    );
    loop_ctx.add_command(datapack, amount.score.clone().remove(1));

    loop_ctx.add_command(
        datapack,
        Command::Function(loop_function_location, None)
            .run()
            .if_score_range(amount.score, Some(1), None),
    );

    datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_ctx);
}
