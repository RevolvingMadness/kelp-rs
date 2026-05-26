use std::num::IntErrorKind;

use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};
use ordered_float::NotNan;

use crate::{
    cst::CSTNumericExpression,
    lower_context::{LowerContext, LowerDataType, LowerError},
    span::{span_of_cst_node, text_range_to_span},
};

#[derive(Clone)]
enum NumericKind {
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_numeric_expression(
    node: CSTNumericExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = span_of_cst_node(&node);

    let value_token = node.fractional_value_token()?;
    let value_text = value_token.text();

    let suffix = node
        .numeric_expression_suffix_token()
        .and_then(|suffix_token| {
            let suffix = suffix_token.text();

            Some(match suffix {
                "b" | "B" => NumericKind::Byte,
                "s" | "S" => NumericKind::Short,
                "i" | "I" => NumericKind::Integer,
                "l" | "L" => NumericKind::Long,
                "f" | "F" => NumericKind::Float,
                "d" | "D" => NumericKind::Double,
                _ => return None,
            })
        });

    Some(
        (match suffix {
            Some(NumericKind::Byte) => match value_text.parse::<i8>() {
                Ok(value) => ParsedExpressionKind::Byte(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooBig(LowerDataType::Byte),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooSmall(LowerDataType::Byte),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Short) => match value_text.parse::<i16>() {
                Ok(value) => ParsedExpressionKind::Short(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooBig(LowerDataType::Short),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooSmall(LowerDataType::Short),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Integer) => match value_text.parse::<i32>() {
                Ok(value) => ParsedExpressionKind::Integer(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooBig(LowerDataType::Integer),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooSmall(LowerDataType::Integer),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Long) => match value_text.parse::<i64>() {
                Ok(value) => ParsedExpressionKind::Long(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooBig(LowerDataType::Long),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error_unit(
                            text_range_to_span(value_token.text_range()),
                            LowerError::ValueTooSmall(LowerDataType::Long),
                        );

                        ParsedExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Float) => {
                let value = value_text.parse().unwrap();

                ParsedExpressionKind::Float(NotNan::new(value).unwrap())
            }
            Some(NumericKind::Double) => {
                let value = value_text.parse().unwrap();

                ParsedExpressionKind::Double(value)
            }
            None => {
                if value_text.contains('.') {
                    let value = value_text.parse().unwrap();

                    ParsedExpressionKind::InferredFloat(value)
                } else {
                    match value_text.parse::<i32>() {
                        Ok(value) => ParsedExpressionKind::InferredInteger(value),
                        Err(error) => match error.kind() {
                            IntErrorKind::PosOverflow => {
                                ctx.add_error_unit(
                                    text_range_to_span(value_token.text_range()),
                                    LowerError::ValueTooBig(LowerDataType::Integer),
                                );

                                ParsedExpressionKind::Invalid
                            }
                            IntErrorKind::NegOverflow => {
                                ctx.add_error_unit(
                                    text_range_to_span(value_token.text_range()),
                                    LowerError::ValueTooSmall(LowerDataType::Integer),
                                );

                                ParsedExpressionKind::Invalid
                            }
                            _ => unreachable!(),
                        },
                    }
                }
            }
        })
        .with_span(span),
    )
}
