use crate::{float, required_inline_whitespace, required_whitespace};
use minecraft_command_types::coordinate::{Coordinates, WorldCoordinate};
use ordered_float::NotNan;
use parser_rs::{
    combinators::{char, choice::choice},
    fn_parser::FnParser,
    stream::Stream,
};

fn parse_local_component<'a>(can_mix: bool) -> impl FnParser<'a, Output = Option<NotNan<f32>>> {
    (move |input: &mut Stream| {
        if !can_mix {
            char('^').parse(input)?;
            return float.optional().parse(input);
        } else {
            if char('^').parse(input).is_some() {
                return float.optional().parse(input);
            }

            let start = input.position;

            if char('~').parse(input).is_some() {
                let result = float.optional().parse(input);

                input.add_validation_error_span(
                    start..input.position,
                    "Cannot mix world and local coordinates",
                );

                return result;
            }

            let value = float(input)?;

            input.add_validation_error_span(
                start..input.position,
                "Cannot mix world and local coordinates",
            );

            Some(Some(value))
        }
    })
    .label("coordinate")
}

pub fn parse_local_coordinates(input: &mut Stream) -> Option<Coordinates> {
    (|input: &mut Stream| {
        let x = parse_local_component(false).parse(input)?;

        required_whitespace(input)?;

        let y = parse_local_component(true).parse(input)?;

        required_whitespace(input)?;

        let z = parse_local_component(true).parse(input)?;

        Some(Coordinates::Local(x, y, z))
    })
    .label("coordinates")
    .parse(input)
}

pub fn parse_world_coordinate<'a>(can_mix: bool) -> impl FnParser<'a, Output = WorldCoordinate> {
    (move |input: &mut Stream| {
        if can_mix && char('^').parse(input).is_some() {
            let start = input.position - 1;

            let value = float
                .optional()
                .map(|value| WorldCoordinate {
                    relative: true,
                    value,
                })
                .parse(input)?;

            input.add_validation_error_span(
                start..input.position,
                "Cannot mix world and local coordinates",
            );

            return Some(value);
        }

        choice((
            |input: &mut Stream| {
                char('~').parse(input)?;
                let value = float.optional().parse(input)?;
                Some(WorldCoordinate {
                    relative: true,
                    value,
                })
            },
            |input: &mut Stream| {
                let value = float(input)?;
                Some(WorldCoordinate {
                    relative: false,
                    value: Some(value),
                })
            },
        ))
        .parse(input)
    })
    .label("coordinate")
}

fn parse_world_coordinates(input: &mut Stream) -> Option<Coordinates> {
    (|input: &mut Stream| {
        let x = parse_world_coordinate(false).parse(input)?;

        required_inline_whitespace(input)?;

        let y = parse_world_coordinate(true).parse(input)?;

        required_inline_whitespace(input)?;

        let z = parse_world_coordinate(true).parse(input)?;

        Some(Coordinates::World(x, y, z))
    })
    .label("coordinates")
    .parse(input)
}

pub fn parse_coordinates(input: &mut Stream) -> Option<Coordinates> {
    choice((
        |input: &mut Stream| parse_world_coordinates(input),
        |input: &mut Stream| parse_local_coordinates(input),
    ))
    .parse(input)
}
