use std::num::IntErrorKind;

use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};
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
) -> Option<ParsedExpressionId> {
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

    let expression = match suffix {
        Some(NumericKind::Byte) => match value_text.parse::<i8>() {
            Ok(value) => ParsedExpression::Byte(value),
            Err(error) => match error.kind() {
                IntErrorKind::PosOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooBig(LowerDataType::Byte),
                    );

                    ParsedExpression::Invalid
                }
                IntErrorKind::NegOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooSmall(LowerDataType::Byte),
                    );

                    ParsedExpression::Invalid
                }
                _ => unreachable!(),
            },
        },
        Some(NumericKind::Short) => match value_text.parse::<i16>() {
            Ok(value) => ParsedExpression::Short(value),
            Err(error) => match error.kind() {
                IntErrorKind::PosOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooBig(LowerDataType::Short),
                    );

                    ParsedExpression::Invalid
                }
                IntErrorKind::NegOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooSmall(LowerDataType::Short),
                    );

                    ParsedExpression::Invalid
                }
                _ => unreachable!(),
            },
        },
        Some(NumericKind::Integer) => match value_text.parse::<i32>() {
            Ok(value) => ParsedExpression::Integer(value),
            Err(error) => match error.kind() {
                IntErrorKind::PosOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooBig(LowerDataType::Integer),
                    );

                    ParsedExpression::Invalid
                }
                IntErrorKind::NegOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooSmall(LowerDataType::Integer),
                    );

                    ParsedExpression::Invalid
                }
                _ => unreachable!(),
            },
        },
        Some(NumericKind::Long) => match value_text.parse::<i64>() {
            Ok(value) => ParsedExpression::Long(value),
            Err(error) => match error.kind() {
                IntErrorKind::PosOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooBig(LowerDataType::Long),
                    );

                    ParsedExpression::Invalid
                }
                IntErrorKind::NegOverflow => {
                    ctx.add_error_unit(
                        text_range_to_span(value_token.text_range()),
                        LowerError::ValueTooSmall(LowerDataType::Long),
                    );

                    ParsedExpression::Invalid
                }
                _ => unreachable!(),
            },
        },
        Some(NumericKind::Float) => {
            let value = value_text.parse().unwrap();

            ParsedExpression::Float(NotNan::new(value).unwrap())
        }
        Some(NumericKind::Double) => {
            let value = value_text.parse().unwrap();

            ParsedExpression::Double(value)
        }
        None => {
            if value_text.contains('.') {
                let value = value_text.parse().unwrap();

                ParsedExpression::InferredFloat(value)
            } else {
                match value_text.parse::<i32>() {
                    Ok(value) => ParsedExpression::InferredInteger(value),
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            ctx.add_error_unit(
                                text_range_to_span(value_token.text_range()),
                                LowerError::ValueTooBig(LowerDataType::Integer),
                            );

                            ParsedExpression::Invalid
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error_unit(
                                text_range_to_span(value_token.text_range()),
                                LowerError::ValueTooSmall(LowerDataType::Integer),
                            );

                            ParsedExpression::Invalid
                        }
                        _ => unreachable!(),
                    },
                }
            }
        }
    };

    Some(ctx.allocator.allocate_expression(span, expression))
}
