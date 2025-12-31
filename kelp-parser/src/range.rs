use crate::{float, integer};
use minecraft_command_types::range::{FloatRange, IntegerRange};
use parser_rs::{FnParser, Stream, literal};

pub fn parse_float_range(input: &mut Stream) -> Option<FloatRange> {
    (|input: &mut Stream| {
        let min = float.optional().parse(input)?;

        let (dots_span, had_dots) = if min.is_some() {
            (None, literal("..").optional().parse(input)?.is_some())
        } else {
            let (span, _) = literal("..").spanned().parse(input)?;
            (Some(span), true)
        };

        let max = if had_dots {
            float.optional().parse(input)?
        } else {
            min
        };

        let range = FloatRange { min, max };

        if min.is_none()
            && max.is_none()
            && let Some(span) = dots_span
        {
            input.add_validation_error_span(
                span,
                "Float range must have a minimum and/or maximum bound",
            );
        }

        Some(range)
    })
    .label("float range")
    .parse(input)
}

pub fn parse_integer_range(input: &mut Stream) -> Option<IntegerRange> {
    (|input: &mut Stream| {
        let min = integer.optional().parse(input)?;

        let (dots_span, had_dots) = if min.is_some() {
            (None, literal("..").optional().parse(input)?.is_some())
        } else {
            let (span, _) = literal("..").spanned().parse(input)?;
            (Some(span), true)
        };

        let max = if had_dots {
            integer.optional().parse(input)?
        } else {
            min
        };

        let range = IntegerRange { min, max };

        if min.is_none()
            && max.is_none()
            && let Some(span) = dots_span
        {
            input.add_validation_error_span(
                span,
                "Integer range must have a minimum and/or maximum bound",
            );
        }

        Some(range)
    })
    .label("integer range")
    .parse(input)
}
