use crate::column_position::parse_column_position;
use crate::command::HighCommand;
use crate::command::data::{parse_data_target, parse_nbt_path};
use crate::command::parse_command;
use crate::coordinate::parse_coordinates;
use crate::entity_selector::parse_entity_selector;
use crate::enums::{
    parse_bossbar_store_type, parse_entity_anchor, parse_heightmap, parse_if_blocks_mode,
    parse_relation, parse_store_type,
};
use crate::expression::{expression, parse_compound};
use crate::range::parse_integer_range;
use crate::resource_location::parse_resource_location;
use crate::{float, identifier, key, newline_whitespace, required_inline_whitespace, whitespace};
use kelp_core::high::block::HighBlockState;
use kelp_core::high::command::execute::facing::HighFacing;
use kelp_core::high::command::execute::positioned::HighPositioned;
use kelp_core::high::command::execute::rotated::HighRotated;
use kelp_core::high::command::execute::subcommand::HighExecuteSubcommand;
use kelp_core::high::command::execute::subcommand::r#if::HighExecuteIfSubcommand;
use kelp_core::high::command::execute::subcommand::store::HighExecuteStoreSubcommand;
use kelp_core::high::item::{HighItemPredicate, HighItemTest, HighOrGroup};
use kelp_core::high::item_source::HighItemSource;
use kelp_core::high::player_score::HighPlayerScore;
use kelp_core::high::score_comparison::HighScoreComparison;
use minecraft_command_types::command::enums::axis::Axis;
use minecraft_command_types::command::enums::numeric_snbt_type::NumericSNBTType;
use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::item::ItemType;
use minecraft_command_types::rotation::Rotation;
use parser_rs::{
    Expectation,
    combinators::{char, choice::choice, literal, suggest_literal},
    fn_parser::FnParser,
    stream::Stream,
};
use std::collections::BTreeSet;

const NEXT_PARAMETER: (&str, Option<&str>) = ("<next>", Some("The next subcommand"));

fn parse_next(input: &mut Stream) -> Option<Box<HighExecuteSubcommand>> {
    required_inline_whitespace.parse(input)?;
    parse_execute_subcommand
        .next_signature_parameter()
        .parse(input)
        .map(Box::new)
}

pub fn parse_block_state<'a>(input: &mut Stream<'a>) -> Option<HighBlockState> {
    let id = parse_resource_location.parse(input)?;
    let block_states = (|input: &mut Stream<'a>| {
        char('[').parse(input)?;
        let block_states = (|input: &mut Stream<'a>| {
            whitespace.parse(input)?;
            let key = identifier("block state key").parse(input)?;
            whitespace.parse(input)?;
            char('=').parse(input)?;
            whitespace.parse(input)?;
            let value = identifier("block state value").parse(input)?;
            whitespace.parse(input)?;
            Some((key, value))
        })
        .separated_by::<_, Vec<_>>(char(','))
        .parse(input)?;
        char(']').parse(input)?;

        Some(block_states)
    })
    .optional()
    .parse(input)?
    .unwrap_or_default();
    let data_tags = parse_compound.optional().parse(input)?;

    Some(HighBlockState {
        id,
        block_states: block_states
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect(),
        data_tags,
    })
}

pub fn parse_item_source(input: &mut Stream) -> Option<HighItemSource> {
    choice((
        |input: &mut Stream| {
            (|input: &mut Stream| {
                suggest_literal("block").parse(input)?;
                required_inline_whitespace.parse(input)
            })
            .optional()
            .parse(input)?;

            let coordinates = parse_coordinates.parse(input)?;

            Some(HighItemSource::Block(coordinates))
        },
        |input: &mut Stream| {
            (|input: &mut Stream| {
                suggest_literal("entity").parse(input)?;
                required_inline_whitespace.parse(input)
            })
            .optional()
            .parse(input)?;

            let selector = parse_entity_selector.parse(input)?;

            Some(HighItemSource::Entity(selector))
        },
    ))
    .parse(input)
}

pub fn parse_item_type(input: &mut Stream) -> Option<ItemType> {
    choice((
        parse_resource_location.map(ItemType::ResourceLocation),
        char('*').map_to(ItemType::Wildcard),
    ))
    .parse(input)
}

pub fn parse_item_test(input: &mut Stream) -> Option<(bool, HighItemTest)> {
    let negated = char('!').optional().parse(input)?.is_some();

    let location = parse_resource_location.parse(input)?;

    let optional_match = (|input: &mut Stream| {
        whitespace.parse(input)?;

        let is_matches = choice((char('=').map_to(true), char('~').map_to(false))).parse(input)?;

        whitespace.parse(input)?;
        let value = expression.parse(input)?;
        Some((is_matches, value))
    })
    .optional()
    .parse(input)?;

    let (is_matches, value) = if let Some((is_matches, value)) = optional_match {
        (is_matches, value)
    } else {
        return Some((negated, HighItemTest::Component(location)));
    };

    let test = if is_matches {
        HighItemTest::ComponentMatches(location, value)
    } else {
        HighItemTest::Predicate(location, value)
    };

    Some((negated, test))
}

pub fn parse_or_group(input: &mut Stream) -> Option<HighOrGroup> {
    parse_item_test
        .separated_by_one(char('|'))
        .map(HighOrGroup)
        .parse(input)
}

pub fn parse_item_predicate(input: &mut Stream) -> Option<HighItemPredicate> {
    let item_type = parse_item_type.parse(input)?;

    let tests = (|input: &mut Stream| {
        char('[').parse(input)?;

        let inner_parser = |input: &mut Stream| {
            whitespace.parse(input)?;
            let or_group = parse_or_group.parse(input)?;
            whitespace.parse(input)?;
            Some(or_group)
        };

        let tests = inner_parser.separated_by(char(',')).parse(input)?;

        char(']').parse(input)?;
        Some(tests)
    })
    .optional()
    .parse(input)?
    .unwrap_or_default();

    Some(HighItemPredicate {
        id: item_type,
        or_groups: tests,
    })
}

pub fn parse_player_score(input: &mut Stream) -> Option<HighPlayerScore> {
    let selector = parse_entity_selector.parse(input)?;
    required_inline_whitespace.parse(input)?;
    let score = identifier("scoreboard objective").parse(input)?;

    Some(HighPlayerScore::new(selector, score.to_string()))
}

pub fn parse_score_comparison_operator(input: &mut Stream) -> Option<ScoreComparisonOperator> {
    choice((
        char('<').map_to(ScoreComparisonOperator::LessThan),
        literal("<=").map_to(ScoreComparisonOperator::LessThanOrEqualTo),
        char('=').map_to(ScoreComparisonOperator::EqualTo),
        char('>').map_to(ScoreComparisonOperator::GreaterThan),
        literal(">=").map_to(ScoreComparisonOperator::GreaterThanOrEqualTo),
    ))
    .parse(input)
}

pub fn parse_score_comparison(input: &mut Stream) -> Option<HighScoreComparison> {
    choice((
        |input: &mut Stream| {
            suggest_literal("matches").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let range = parse_integer_range.parse(input)?;

            Some(HighScoreComparison::Range(range))
        },
        |input: &mut Stream| {
            let operator = parse_score_comparison_operator.parse(input)?;
            required_inline_whitespace.parse(input)?;
            let score = parse_player_score.parse(input)?;
            Some(HighScoreComparison::Score(operator, score))
        },
    ))
    .parse(input)
}

pub fn parse_execute_if_subcommand(input: &mut Stream) -> Option<HighExecuteIfSubcommand> {
    choice((
        (|input: &mut Stream| {
            suggest_literal("biome").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let coordinates = parse_coordinates.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let biome = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Biome(coordinates, biome, next))
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("blocks").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let start = parse_coordinates.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let end = parse_coordinates.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let destination = parse_coordinates.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let mode = parse_if_blocks_mode
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Blocks(
                start,
                end,
                destination,
                mode,
                next,
            ))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("block").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let coordinates = parse_coordinates.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let predicate = parse_block_state.next_signature_parameter().parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Block(coordinates, predicate, next))
        })
        .signature(2),
        (|input: &mut Stream| {
            suggest_literal("data").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let target = parse_data_target(true)
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let path = parse_nbt_path.next_signature_parameter().parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Data(target, path, next))
        })
        .signature(3),
        (|input: &mut Stream| {
            suggest_literal("dimension").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let dimension = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Dimension(dimension, next))
        })
        .signature(4),
        (|input: &mut Stream| {
            suggest_literal("entity").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Entity(selector, next))
        })
        .signature(5),
        (|input: &mut Stream| {
            suggest_literal("function").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let function = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;

            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Function(function, next))
        })
        .signature(6),
        (|input: &mut Stream| {
            suggest_literal("items").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let source = parse_item_source.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let slot = key("item slot")
                .label("slot")
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let item_predicate = parse_item_predicate
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Items(
                source,
                slot.to_string(),
                item_predicate,
                next,
            ))
        })
        .signature(7),
        (|input: &mut Stream| {
            suggest_literal("loaded").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let column_position = parse_column_position
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Loaded(column_position, next))
        })
        .signature(8),
        (|input: &mut Stream| {
            suggest_literal("predicate").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let predicate = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Predicate(predicate, next))
        })
        .signature(9),
        choice(((|input: &mut Stream| {
            suggest_literal("score").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let objective = identifier("scoreboard objective")
                .next_signature_parameter()
                .parse(input)?;
            let score = HighPlayerScore::new(selector, objective.to_string());

            required_inline_whitespace.parse(input)?;
            let comparison = choice((
                (|input: &mut Stream| {
                    let op = parse_score_comparison_operator
                        .next_signature_parameter()
                        .parse(input)?;
                    required_inline_whitespace.parse(input)?;
                    let source_selector = parse_entity_selector
                        .next_signature_parameter()
                        .parse(input)?;
                    required_inline_whitespace.parse(input)?;
                    let source_objective = identifier("scoreboard objective")
                        .next_signature_parameter()
                        .parse(input)?;
                    let source_score =
                        HighPlayerScore::new(source_selector, source_objective.to_string());

                    Some(HighScoreComparison::Score(op, source_score))
                })
                .signature(0),
                (|input: &mut Stream| {
                    suggest_literal("matches").parse(input)?;
                    required_inline_whitespace.parse(input)?;
                    let range = parse_integer_range
                        .next_signature_parameter()
                        .parse(input)?;

                    Some(HighScoreComparison::Range(range))
                })
                .signature(1),
            ))
            .signatures(&[
                (
                    "<op> <target> <targetObjective> [<next>]",
                    &[
                        ("<op>", Some("The comparison operator to use when checking")),
                        (
                            "<target>",
                            Some("The target entity of the target objective"),
                        ),
                        ("<targetObjective>", Some("The target objective")),
                    ],
                    Some("Compares the scores of two entities"),
                ),
                (
                    "matches <range> [<next>]",
                    &[("<range>", Some("The range the score must be within"))],
                    Some("Compares the scores using <range>"),
                ),
            ])
            .parse(input)?;

            let next = parse_next.optional().parse(input)?;

            Some(HighExecuteIfSubcommand::Score(score, comparison, next))
        })
        .signature(10),)),
    ))
    .signatures(&[
        (
            "biome <pos> <biome> [<next>]",
            &[
                ("<pos>", Some("The position of the biome")),
                ("<biome>", Some("The id of the biome")),
                NEXT_PARAMETER,
            ],
            Some("Checks for a specific biome in a given position."),
        ),
        (
            "blocks <start> <end> <destination> all|masked [<next>]",
            &[
                ("<start>", Some("First corner of the cube of blocks")),
                ("<end>", Some("Second corner of the cube of blocks")),
                (
                    "<destination>",
                    Some("The block to compare the cube of blocks to"),
                ),
                ("all|masked", Some("Compare all blocks or masked blocks")),
                NEXT_PARAMETER,
            ],
            Some("Compares the blocks in two equally sized volumes."),
        ),
        (
            "block <pos> <block_predicate> [<next>]",
            &[
                ("<pos>", Some("The position of the block")),
                ("<block_predicate>", Some("The block predicate")),
                NEXT_PARAMETER,
            ],
            Some("Compares the block at <pos> to <block_predicate>"),
        ),
        (
            "data <source> <path> [<next>]",
            &[
                ("<source>", Some("The source of nbt")),
                ("<path>", Some("The path of nbt")),
                NEXT_PARAMETER,
            ],
            Some("Checks whether <path> exists on <source>"),
        ),
        (
            "dimension <dimension> [<next>]",
            &[
                ("<dimension>", Some("The id of the dimension")),
                NEXT_PARAMETER,
            ],
            Some("Checks if the executor is in <dimension>"),
        ),
        (
            "entity <entity_predicate> [<next>]",
            &[
                ("<entity_predicate>", Some("The predicate to check")),
                NEXT_PARAMETER,
            ],
            Some("Checks if <entity_predicate> succeeds"),
        ),
        (
            "function <function> <next>",
            &[
                ("<function>", Some("The id of the function to run")),
                NEXT_PARAMETER,
            ],
            Some("Checks if a function succeeded.\nNote: Bug MC-267799 requires this to have a following subcommand"), // TODO add feature to toggle this
        ),
        (
            "items <source> <slot> <item_predicate> [<next>]",
            &[
                ("<source>", Some("The source of items to check")),
                ("<slot>", Some("The slot(s) to check")),
                ("<item_predicate>", Some("The item predicate to use")),
                NEXT_PARAMETER,
            ],
            Some("Checks for a matching item within the <slot> of <source>"),
        ),
        (
            "loaded <pos> [<next>]",
            &[("<pos>", Some("The position to check")), NEXT_PARAMETER],
            Some("Checks if <pos> is loaded"),
        ),
        (
            "predicate <predicate> [<next>]",
            &[
                ("<predicate>", Some("The predicate to check")),
                NEXT_PARAMETER,
            ],
            Some("Runs <predicate>"),
        ),
        (
            "score <source> <source_objective> <comparison> [<next>]",
            &[
                ("<source>", Some("The entity of the objective")),
                (
                    "<source_objective>",
                    Some("The scoreboard objective to check"),
                ),
                ("<comparison>", Some("The comparison method")),
                NEXT_PARAMETER,
            ],
            Some("Checks if a score succeeds with <comparison>"),
        ),
    ])
    .parse(input)
}

pub fn parse_numeric_snbt_type(input: &mut Stream) -> Option<NumericSNBTType> {
    choice((
        suggest_literal("byte").map_to(NumericSNBTType::Byte),
        suggest_literal("double").map_to(NumericSNBTType::Double),
        suggest_literal("float").map_to(NumericSNBTType::Float),
        suggest_literal("int").map_to(NumericSNBTType::Integer),
        suggest_literal("long").map_to(NumericSNBTType::Long),
        suggest_literal("short").map_to(NumericSNBTType::Short),
    ))
    .parse(input)
}

pub fn parse_execute_store_command(input: &mut Stream) -> Option<HighExecuteStoreSubcommand> {
    const PATH_PARAMETER: (&str, Option<&str>) =
        ("<path>", Some("Where to store the value to in the SNBT"));
    const TYPE_PARAMETER: (&str, Option<&str>) = ("<type>", Some("The type of SNBT to store as"));
    const SCALE_PARAMETER: (&str, Option<&str>) =
        ("<scale>", Some("How much to scale the value to be stored"));

    choice((
        (|input: &mut Stream| {
            let target = parse_data_target(true)
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let path = parse_nbt_path.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let numeric_snbt_type = parse_numeric_snbt_type
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let scale = float.next_signature_parameter().parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteStoreSubcommand::Data(
                target,
                path,
                numeric_snbt_type,
                scale,
                next,
            ))
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("bossbar").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let location = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let bossbar_store_type = parse_bossbar_store_type
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteStoreSubcommand::Bossbar(
                location,
                bossbar_store_type,
                next,
            ))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("score").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let objective = identifier("objective")
                .next_signature_parameter()
                .parse(input)?;
            let score = HighPlayerScore::new(selector, objective.to_string());
            let next = parse_next.parse(input)?;

            Some(HighExecuteStoreSubcommand::Score(score, next))
        })
        .signature(2),
    ))
    .signatures(&[
        (
            "<target> <path> <type> <scale> <next>",
            &[
                ("<target>", Some("The data target to store into")),
                PATH_PARAMETER,
                TYPE_PARAMETER,
                SCALE_PARAMETER,
                NEXT_PARAMETER,
            ],
            Some("Stores the resulting value into block"),
        ),
        (
            "bossbar <id> value|max <next>",
            &[
                ("<id>", Some("The id of the bossbar to store into")),
                (
                    "value|max",
                    Some("Store into the value or max value of the bossbar"),
                ),
                NEXT_PARAMETER,
            ],
            Some("Stores the resulting value into the value or max value of a bossbar"),
        ),
        (
            "score <targets> <objective> <next>",
            &[
                ("<targets>", Some("The entity of the objective")),
                ("<objective>", Some("The objective to store into")),
                NEXT_PARAMETER,
            ],
            Some("Stores the resulting value into a scoreboard"),
        ),
    ])
    .parse(input)
}

pub fn parse_axes(input: &mut Stream) -> Option<BTreeSet<Axis>> {
    let mut has_x = false;
    let mut has_y = false;
    let mut has_z = false;

    loop {
        let char_start_pos = input.position;

        if !has_x {
            input.suggest(char_start_pos, &Expectation::Char('x'));
        }
        if !has_y {
            input.suggest(char_start_pos, &Expectation::Char('y'));
        }
        if !has_z {
            input.suggest(char_start_pos, &Expectation::Char('z'));
        }

        let Some(c) = input.current_char() else {
            break;
        };

        if !c.is_alphabetic() {
            break;
        }

        input.consume_char();
        let span = char_start_pos..input.position;

        match c {
            'x' => {
                if has_x {
                    input.add_validation_error_span(
                        span,
                        "This axis is already included in the swizzle.",
                    );
                }
                has_x = true;
            }
            'y' => {
                if has_y {
                    input.add_validation_error_span(
                        span,
                        "This axis is already included in the swizzle.",
                    );
                }
                has_y = true;
            }
            'z' => {
                if has_z {
                    input.add_validation_error_span(
                        span,
                        "This axis is already included in the swizzle.",
                    );
                }
                has_z = true;
            }
            _ => {
                input.add_validation_error_span(span, "Invalid axis.");
            }
        }
    }

    if !has_x && !has_y && !has_z {
        return input.fail_expected(&Expectation::Custom("axes"));
    }

    let mut swizzle = BTreeSet::new();
    if has_x {
        swizzle.insert(Axis::X);
    }
    if has_y {
        swizzle.insert(Axis::Y);
    }
    if has_z {
        swizzle.insert(Axis::Z);
    }

    Some(swizzle)
}

pub fn parse_facing(input: &mut Stream) -> Option<HighFacing> {
    choice((
        parse_coordinates
            .next_signature_parameter()
            .signature(0)
            .map(HighFacing::Position),
        (|input: &mut Stream| {
            suggest_literal("entity").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let anchor = parse_entity_anchor
                .next_signature_parameter()
                .parse(input)?;

            Some(HighFacing::Entity(selector, anchor))
        })
        .signature(1),
    ))
    .signatures(&[
        (
            "<pos>",
            &[("<pos>", Some("The position to face towards"))],
            Some("Faces towards <pos>"),
        ),
        (
            "entity <targets> <anchor>",
            &[
                ("<targets>", Some("The entity to face towards")),
                ("<anchor>", Some("The anchor to use")),
            ],
            Some("Faces towards <targets> with a specified anchor"),
        ),
    ])
    .parse(input)
}

pub fn parse_positioned(input: &mut Stream) -> Option<HighPositioned> {
    choice((
        parse_coordinates
            .next_signature_parameter()
            .signature(0)
            .map(HighPositioned::Position),
        (|input: &mut Stream| {
            suggest_literal("as").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;

            Some(HighPositioned::As(selector))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("over").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let heightmap = parse_heightmap.next_signature_parameter().parse(input)?;

            Some(HighPositioned::Over(heightmap))
        })
        .signature(2),
    ))
    .signatures(&[
        (
            "<pos>",
            &[("<pos>", Some("The coordinates to position to"))],
            Some("Positions the executor to the specified coordinates"),
        ),
        (
            "as <entities>",
            &[("<entities>", None)],
            Some("Positions the executor as <entities>"),
        ),
        (
            "over <heightmap>",
            &[("<heightmap>", None)],
            Some("Positions the executor over <heightmap>"),
        ),
    ])
    .parse(input)
}

pub fn parse_rotated(input: &mut Stream) -> Option<HighRotated> {
    choice((
        (|input: &mut Stream| {
            let yaw = float.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let pitch = float.next_signature_parameter().parse(input)?;

            Some(HighRotated::Rotation(Rotation(yaw, pitch)))
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("as").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;

            Some(HighRotated::As(selector))
        })
        .signature(1),
    ))
    .signatures(&[
        (
            "<yaw> <pitch>",
            &[("<yaw>", Some("The yaw")), ("<pitch>", Some("The pitch"))],
            Some("Rotates the executor to have <yaw> and <pitch>"),
        ),
        (
            "as <entities>",
            &[("<entities>", Some("The entity whose rotation will be used"))],
            Some("Rotates the executor to how <entities> is rotated"),
        ),
    ])
    .parse(input)
}

pub fn parse_execute_subcommand(input: &mut Stream) -> Option<HighExecuteSubcommand> {
    choice((
        (|input: &mut Stream| {
            suggest_literal("align").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let swizzle = parse_axes.next_signature_parameter().parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Align(swizzle, next))
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("anchored").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let anchor = parse_entity_anchor
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Anchored(anchor, next))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("as").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::As(selector, next))
        })
        .signature(2),
        (|input: &mut Stream| {
            suggest_literal("at").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let selector = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::At(selector, next))
        })
        .signature(3),
        (|input: &mut Stream| {
            suggest_literal("facing").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let facing = parse_facing.next_signature_parameter().parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Facing(facing, next))
        })
        .signature(4),
        (|input: &mut Stream| {
            suggest_literal("in").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let location = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::In(location, next))
        })
        .signature(5),
        (|input: &mut Stream| {
            suggest_literal("on").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let relation = parse_relation.next_signature_parameter().parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::On(relation, next))
        })
        .signature(6),
        (|input: &mut Stream| {
            suggest_literal("positioned").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let positioned = parse_positioned.parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Positioned(positioned, next))
        })
        .signature(7),
        (|input: &mut Stream| {
            suggest_literal("rotated").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let rotated = parse_rotated.next_signature_parameter().parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Rotated(rotated, next))
        })
        .signature(8),
        (|input: &mut Stream| {
            suggest_literal("summon").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let location = parse_resource_location
                .next_signature_parameter()
                .parse(input)?;
            let next = parse_next.parse(input)?;

            Some(HighExecuteSubcommand::Summon(location, next))
        })
        .signature(9),
        (|input: &mut Stream| {
            let inverted = choice((
                suggest_literal("if").map_to(false),
                suggest_literal("unless").map_to(true),
            ))
            .parse(input)?;
            required_inline_whitespace.parse(input)?;
            let if_subcommand = parse_execute_if_subcommand
                .next_signature_parameter()
                .parse(input)?;

            Some(HighExecuteSubcommand::If(inverted, if_subcommand))
        })
        .signature(10),
        (|input: &mut Stream| {
            suggest_literal("store").parse(input)?;
            required_inline_whitespace.parse(input)?;
            let store_type = parse_store_type.next_signature_parameter().parse(input)?;
            required_inline_whitespace.parse(input)?;
            let store_subcommand = parse_execute_store_command
                .next_signature_parameter()
                .parse(input)?;
            Some(HighExecuteSubcommand::Store(store_type, store_subcommand))
        })
        .signature(11),
        (|input: &mut Stream| {
            suggest_literal("run").parse(input)?;
            required_inline_whitespace.parse(input)?;

            let commands: Vec<HighCommand> = choice((
                |input: &mut Stream| {
                    let start = input.position;

                    char('{').parse(input)?;
                    whitespace.parse(input)?;

                    let result = parse_command
                        .separated_by::<_, Vec<_>>(newline_whitespace("end of command"))
                        .parse(input)?;

                    whitespace(input)?;

                    char('}').parse(input)?;

                    if result.is_empty() {
                        input.add_validation_error_span(
                            start..input.position,
                            "There must be at least one command.",
                        );
                    }

                    Some(result)
                },
                |input: &mut Stream| parse_command.parse(input).map(|v| vec![v]),
            ))
            .next_signature_parameter()
            .parse(input)?;

            Some(HighExecuteSubcommand::Run(commands))
        })
        .signature(12),
        |input: &mut Stream| {
            let start = input.position;

            char('{').parse(input)?;
            whitespace.parse(input)?;
            let result = parse_execute_subcommand
                .separated_by::<_, Vec<_>>(newline_whitespace("end of execute subcommand"))
                .parse(input)?;

            whitespace.parse(input)?;
            char('}').parse(input)?;

            if result.is_empty() {
                input.add_validation_error_span(
                    start..input.position,
                    "There must be at least one subcommand.",
                );
            }

            Some(HighExecuteSubcommand::Multiple(result))
        },
    ))
    .signatures(&[
        (
            "align <axes> <next>",
            &[
                ("<axes>", Some("The axes to align the entity")),
                NEXT_PARAMETER,
            ],
            Some("Aligns the executors position to the specified axes"),
        ),
        (
            "anchored <anchor> <next>",
            &[("<anchor>", Some("The anchor")), NEXT_PARAMETER],
            Some("Changes the executors anchor to <anchor>"),
        ),
        (
            "as <targets> <next>",
            &[
                ("<targets>", Some("The entity to execute as")),
                NEXT_PARAMETER,
            ],
            Some("Changes the executor to <targets>"),
        ),
        (
            "at <targets> <next>",
            &[
                ("<targets>", Some("The entity to execute at")),
                NEXT_PARAMETER,
            ],
            Some("Changes the position, rotation, and dimenson to <targets>"),
        ),
        (
            "facing <facing> <next>",
            &[("<facing>", None), NEXT_PARAMETER],
            Some("Changes where the executor is facing to <facing>"),
        ),
        (
            "in <dimension> <next>",
            &[
                ("<dimension>", Some("The dimension to execute in")),
                NEXT_PARAMETER,
            ],
            Some("Changes the executors dimension to <dimension>"),
        ),
        (
            "on <relation> <next>",
            &[
                ("<relation>", Some("The relation to execute on")),
                NEXT_PARAMETER,
            ],
            Some("Changes the executor to other entities specified by the <relation>"),
        ),
        (
            "positioned <position> <next>",
            &[("<position>", None), NEXT_PARAMETER],
            Some("Changes the executors position to <position>"),
        ),
        (
            "rotated <rotation> <next>",
            &[
                ("<rotation>", Some("The rotation to execute as")),
                NEXT_PARAMETER,
            ],
            Some("Changes the executors rotation to <rotation>"),
        ),
        (
            "summon <entity> <next>",
            &[
                ("<entity>", Some("The entity type to summon")),
                NEXT_PARAMETER,
            ],
            Some("Summons <entity> and changes the executor to that entity"),
        ),
        (
            "if|unless <condition>",
            &[("<condition>", None)],
            Some("Runs a condition and only continues if it succeeded"),
        ),
        (
            "store <result|success> <target> <next>",
            &[
                (
                    "<result|success>",
                    Some("Store the result or if it succeeded"),
                ),
                ("<target>", None),
            ],
            Some("Stores the result or success of a command into <target>"),
        ),
        (
            "run <command>",
            &[("<command>", None)],
            Some("Runs a command"),
        ),
    ])
    .label("execute subcommnd")
    .parse(input)
}

pub fn parse_execute_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("execute").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;
        parse_execute_subcommand
            .next_signature_parameter()
            .parse(input)
            .map(HighCommand::Execute)
    })
    .signature(0)
    .signatures(&[("execute <subcommand>", &[("<subcommand>", None)], None)])
    .parse(input)
}
