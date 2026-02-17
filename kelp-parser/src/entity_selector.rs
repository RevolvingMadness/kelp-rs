use crate::enums::{parse_gamemode, parse_sort};
use crate::expression::expression;
use crate::range::{parse_float_range, parse_integer_range};
use crate::resource_location::parse_resource_location;
use crate::{boolean, float, identifier, integer, whitespace};
use kelp_core::high::entity_selector::{HighEntitySelector, option::HighEntitySelectorOption};
use minecraft_command_types::entity_selector::{AdvancementChoiceType, EntitySelectorVariable};
use parser_rs::{
    Expectation,
    combinators::{char, choice::choice},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};
use std::collections::BTreeMap;
use std::hash::Hash;

fn parse_key_value<'a, K, V>(
    mut key_parser: impl FnParser<'a, Output = K> + 'a,
    mut value_parser: impl FnParser<'a, Output = V> + 'a,
) -> impl FnParser<'a, Output = BTreeMap<K, V>>
where
    K: Hash + Ord,
{
    move |input: &mut Stream<'a>| {
        char('{').parse(input)?;
        whitespace(input)?;
        let pairs = (|input: &mut Stream<'a>| {
            whitespace(input)?;
            let key = key_parser.parse(input)?;
            whitespace(input)?;
            char('=').parse(input)?;
            whitespace(input)?;
            let value = value_parser.parse(input)?;
            whitespace(input)?;
            Some((key, value))
        })
        .separated_by::<_, Vec<_>>(char(','))
        .parse(input)?;

        char('}').parse(input)?;

        Some(pairs.into_iter().collect())
    }
}

#[derive(Default)]
struct EntitySelectorOptions {
    contains_x: bool,
    contains_y: bool,
    contains_z: bool,
    contains_distance: bool,
    contains_dx: bool,
    contains_dy: bool,
    contains_dz: bool,
    contains_x_rotation: bool,
    contains_y_rotation: bool,
    contains_scores: bool,
    contains_team_not_inverted: bool,
    contains_name_not_inverted: bool,
    contains_type_not_inverted: bool,
    contains_gamemode_not_inverted: bool,
    contains_level: bool,
    contains_advancements: bool,
    contains_limit: bool,
    contains_sort: bool,
}

impl EntitySelectorOptions {
    pub fn get_expectations(&self) -> Vec<(usize, Expectation)> {
        let mut expectations = Vec::new();

        if !self.contains_x {
            expectations.push((1, Expectation::Literal("x")));
        }

        if !self.contains_y {
            expectations.push((1, Expectation::Literal("y")));
        }

        if !self.contains_z {
            expectations.push((1, Expectation::Literal("z")));
        }

        if !self.contains_distance {
            expectations.push((8, Expectation::Literal("distance")));
        }

        if !self.contains_dx {
            expectations.push((2, Expectation::Literal("dx")));
        }

        if !self.contains_dy {
            expectations.push((2, Expectation::Literal("dy")));
        }

        if !self.contains_dz {
            expectations.push((2, Expectation::Literal("dz")));
        }

        if !self.contains_x_rotation {
            expectations.push((10, Expectation::Literal("x_rotation")));
        }

        if !self.contains_y_rotation {
            expectations.push((10, Expectation::Literal("y_rotation")));
        }

        if !self.contains_scores {
            expectations.push((6, Expectation::Literal("scores")));
        }

        expectations.push((3, Expectation::Literal("tag")));

        expectations.push((4, Expectation::Literal("team")));

        expectations.push((4, Expectation::Literal("name")));

        expectations.push((4, Expectation::Literal("type")));

        expectations.push((9, Expectation::Literal("predicate")));

        expectations.push((3, Expectation::Literal("nbt")));

        expectations.push((8, Expectation::Literal("gamemode")));

        if !self.contains_level {
            expectations.push((5, Expectation::Literal("level")));
        }

        if !self.contains_advancements {
            expectations.push((12, Expectation::Literal("advancements")));
        }

        if !self.contains_limit {
            expectations.push((5, Expectation::Literal("limit")));
        }

        if !self.contains_sort {
            expectations.push((4, Expectation::Literal("sort")));
        }

        expectations
    }
}

fn parse_entity_selector_option<'a>(
    input: &mut Stream<'a>,
    options: &mut EntitySelectorOptions,
) -> Option<HighEntitySelectorOption> {
    let (key_span, key) = identifier("entity selector option")
        .spanned()
        .label("entity selector option key")
        .on_error(|input: &mut Stream| {
            for (len, expectation) in options.get_expectations() {
                input.add_suggestion_range(input.position..input.position + len, &expectation);
            }
        })
        .syntax(SemanticTokenKind::Variable)
        .parse(input)?;

    let equals = |input: &mut Stream| {
        whitespace(input)?;
        char('=').parse(input)?;
        whitespace(input)
    };

    match key {
        "x" => {
            if options.contains_x {
                input.add_validation_error_span(
                    key_span,
                    "The 'x' key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_x = true;

            Some(HighEntitySelectorOption::X(value))
        }
        "y" => {
            if options.contains_y {
                input.add_validation_error_span(
                    key_span,
                    "The 'y' key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_y = true;

            Some(HighEntitySelectorOption::Y(value))
        }
        "z" => {
            if options.contains_z {
                input.add_validation_error_span(
                    key_span,
                    "The 'z' key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_z = true;

            Some(HighEntitySelectorOption::Z(value))
        }
        "distance" => {
            if options.contains_distance {
                input.add_validation_error_span(
                    key_span,
                    "The 'distance' key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = parse_float_range(input)?;

            options.contains_distance = true;

            Some(HighEntitySelectorOption::Distance(value))
        }
        "dx" | "distance_x" => {
            if options.contains_dx {
                input.add_validation_error_span(
                    key_span,
                    "The distance x key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_dx = true;

            Some(HighEntitySelectorOption::DistanceX(value))
        }
        "dy" | "distance_y" => {
            if options.contains_dy {
                input.add_validation_error_span(
                    key_span,
                    "The distance y key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_dy = true;

            Some(HighEntitySelectorOption::DistanceY(value))
        }
        "dz" | "distance_z" => {
            if options.contains_dz {
                input.add_validation_error_span(
                    key_span,
                    "The distance z key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = float(input)?;

            options.contains_dz = true;

            Some(HighEntitySelectorOption::DistanceZ(value))
        }
        "x_rotation" | "x_rot" => {
            if options.contains_x_rotation {
                input.add_validation_error_span(
                    key_span,
                    "The x rotation key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = parse_float_range(input)?;

            options.contains_x_rotation = true;

            Some(HighEntitySelectorOption::XRotation(value))
        }
        "y_rotation" | "y_rot" => {
            if options.contains_y_rotation {
                input.add_validation_error_span(
                    key_span,
                    "The y rotation key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = parse_float_range(input)?;

            options.contains_y_rotation = true;

            Some(HighEntitySelectorOption::YRotation(value))
        }
        "scores" => {
            if options.contains_scores {
                input.add_validation_error_span(
                    key_span,
                    "The 'scores' key cannot be used more than once",
                );
            }

            equals(input)?;

            let value = parse_key_value(
                identifier("score name").map(ToString::to_string),
                parse_integer_range,
            )
            .parse(input)?;

            options.contains_scores = true;

            Some(HighEntitySelectorOption::Scores(value))
        }
        "tag" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let tag = identifier("tag name")
                .map(ToString::to_string)
                .label("tag name")
                .parse(input)?;

            Some(HighEntitySelectorOption::Tag(inverted, tag))
        }
        "team" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let team = identifier("team name")
                .map(ToString::to_string)
                .label("team name")
                .parse(input)?;

            if !inverted {
                if options.contains_team_not_inverted {
                    input.add_validation_error_span(key_span, "This option must be inverted.");
                }

                options.contains_team_not_inverted = true;
            }

            Some(HighEntitySelectorOption::Team(inverted, team))
        }
        "name" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let name = identifier("entity name")
                .map(ToString::to_string)
                .label("entity name")
                .parse(input)?;

            if !inverted {
                if options.contains_name_not_inverted {
                    input.add_validation_error_span(key_span, "This option must be inverted.");
                }

                options.contains_name_not_inverted = true;
            }

            Some(HighEntitySelectorOption::Name(inverted, name))
        }
        "type" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let type_ = parse_resource_location(input)?;

            if !inverted {
                if options.contains_type_not_inverted {
                    input.add_validation_error_span(key_span, "This option must be inverted.");
                }

                options.contains_type_not_inverted = true;
            }

            Some(HighEntitySelectorOption::Type(inverted, type_))
        }
        "predicate" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let predicate = parse_resource_location(input)?;

            Some(HighEntitySelectorOption::Predicate(inverted, predicate))
        }
        "nbt" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let nbt = expression(input)?;

            Some(HighEntitySelectorOption::Nbt(inverted, Box::new(nbt)))
        }
        "gamemode" => {
            equals(input)?;

            let inverted = char('!').optional().parse(input)?.is_some();

            let gamemode = parse_gamemode(input)?;

            if !inverted {
                if options.contains_gamemode_not_inverted {
                    input.add_validation_error_span(key_span, "This option must be inverted.");
                }

                options.contains_gamemode_not_inverted = true;
            }

            Some(HighEntitySelectorOption::Gamemode(inverted, gamemode))
        }
        "level" => {
            if options.contains_level {
                input.add_validation_error_span(
                    key_span,
                    "The 'level' key cannot be used more than once",
                );
            }

            equals(input)?;

            let level = parse_integer_range(input)?;

            options.contains_level = true;

            Some(HighEntitySelectorOption::Level(level))
        }
        "advancements" => {
            if options.contains_advancements {
                input.add_validation_error_span(
                    key_span,
                    "The 'advancements' key cannot be used more than once",
                );
            }

            equals(input)?;

            let advancements = parse_key_value(
                parse_resource_location,
                choice((
                    boolean.map(AdvancementChoiceType::Boolean),
                    parse_key_value(
                        identifier("advancement criterion").map(ToString::to_string),
                        boolean,
                    )
                    .map(AdvancementChoiceType::Criterion),
                )),
            )
            .parse(input)?;

            options.contains_advancements = true;

            Some(HighEntitySelectorOption::Advancements(advancements))
        }
        "limit" => {
            if options.contains_limit {
                input.add_validation_error_span(
                    key_span,
                    "The 'limit' key cannot be used more than once",
                );
            }

            equals(input)?;

            let limit = integer(input)?;

            options.contains_limit = true;

            Some(HighEntitySelectorOption::Limit(limit))
        }
        "sort" => {
            if options.contains_sort {
                input.add_validation_error_span(
                    key_span,
                    "The 'sort' key cannot be used more than once",
                );
            }

            equals(input)?;

            let sort = parse_sort(input)?;

            options.contains_sort = true;

            Some(HighEntitySelectorOption::Sort(sort))
        }
        _ => {
            input.add_validation_error_span(key_span, "Invalid entity selector option");

            for (len, expectation) in options.get_expectations() {
                input.add_suggestion_range(key_span.start..key_span.start + len, &expectation);
            }

            Some(HighEntitySelectorOption::Limit(1))
        }
    }
}

pub fn parse_entity_selector(input: &mut Stream) -> Option<HighEntitySelector> {
    choice((
        |input: &mut Stream| {
            let start = input.position;

            char('@').parse(input)?;

            let variable = if let Some(variable) = input.current_char() {
                if !variable.is_ascii_alphabetic() {
                    input.add_suggestion(input.position, &Expectation::Char('p'));
                    input.add_suggestion(input.position, &Expectation::Char('r'));
                    input.add_suggestion(input.position, &Expectation::Char('a'));
                    input.add_suggestion(input.position, &Expectation::Char('e'));
                    input.add_suggestion(input.position, &Expectation::Char('s'));
                    input.add_suggestion(input.position, &Expectation::Char('n'));

                    return input.fail_expected(&Expectation::Custom("entity selector variable"));
                }

                input.position += 1;

                match variable {
                    'p' => EntitySelectorVariable::P,
                    'r' => EntitySelectorVariable::R,
                    'a' => EntitySelectorVariable::A,
                    'e' => EntitySelectorVariable::E,
                    's' => EntitySelectorVariable::S,
                    'n' => EntitySelectorVariable::N,
                    _ => {
                        input.add_validation_error_span(
                            input.position - 1..input.position,
                            "Invalid entity selector variable",
                        );

                        EntitySelectorVariable::S
                    }
                }
            } else {
                input.add_suggestion(input.position, &Expectation::Char('p'));
                input.add_suggestion(input.position, &Expectation::Char('r'));
                input.add_suggestion(input.position, &Expectation::Char('a'));
                input.add_suggestion(input.position, &Expectation::Char('e'));
                input.add_suggestion(input.position, &Expectation::Char('s'));
                input.add_suggestion(input.position, &Expectation::Char('n'));

                return input.fail_expected(&Expectation::Custom("entity selector variable"));
            };

            input.add_syntax_from(start, SemanticTokenKind::Variable);

            let options = (|input: &mut Stream| {
                char('[').parse(input)?;

                let mut options = EntitySelectorOptions::default();

                let options = (|input: &mut Stream| {
                    whitespace(input)?;
                    let result = parse_entity_selector_option(input, &mut options)?;
                    whitespace(input)?;
                    Some(result)
                })
                .separated_by(char(','))
                .parse(input)?;

                char(']').parse(input)?;

                Some(options)
            })
            .optional()
            .parse(input)?;

            Some(HighEntitySelector::Variable(
                variable,
                options.unwrap_or_default(),
            ))
        },
        |input: &mut Stream| {
            let name = identifier("entity name")
                .syntax(SemanticTokenKind::Variable)
                .parse(input)
                .map(ToString::to_string)?;

            Some(HighEntitySelector::Name(name))
        },
    ))
    .label("entity selector")
    .parse(input)
}
