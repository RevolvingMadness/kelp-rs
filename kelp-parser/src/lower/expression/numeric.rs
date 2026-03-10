use std::num::IntErrorKind;

use kelp_core::{
    data_type::DataTypeKind,
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
};
use ordered_float::NotNan;

use crate::{
    cst::CSTNumericExpression,
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
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
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
            Some(NumericKind::Byte) => {
                let value = match value_text.parse::<i8>() {
                    Ok(value) => value,
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooLarge(DataTypeKind::Byte),
                            );

                            i8::MAX
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooSmall(DataTypeKind::Byte),
                            );

                            i8::MIN
                        }
                        _ => unreachable!(),
                    },
                };

                ExpressionKind::Byte(value)
            }
            Some(NumericKind::Short) => {
                let value = match value_text.parse::<i16>() {
                    Ok(value) => value,
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooLarge(DataTypeKind::Short),
                            );

                            i16::MAX
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooSmall(DataTypeKind::Short),
                            );

                            i16::MIN
                        }
                        _ => unreachable!(),
                    },
                };

                ExpressionKind::Short(value)
            }
            Some(NumericKind::Integer) => {
                let value = match value_text.parse::<i32>() {
                    Ok(value) => value,
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooLarge(DataTypeKind::Integer),
                            );

                            i32::MAX
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooSmall(DataTypeKind::Integer),
                            );

                            i32::MIN
                        }
                        _ => unreachable!(),
                    },
                };

                ExpressionKind::Integer(value)
            }
            Some(NumericKind::Long) => {
                let value = match value_text.parse::<i64>() {
                    Ok(value) => value,
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooLarge(DataTypeKind::Long),
                            );

                            i64::MAX
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error(
                                text_range_to_span(value_token.text_range()),
                                SemanticAnalysisError::ValueTooSmall(DataTypeKind::Long),
                            );

                            i64::MIN
                        }
                        _ => unreachable!(),
                    },
                };

                ExpressionKind::Long(value)
            }
            Some(NumericKind::Float) => {
                let value = value_text.parse().unwrap();

                ExpressionKind::Float(NotNan::new(value).unwrap())
            }
            Some(NumericKind::Double) => {
                let value = value_text.parse().unwrap();

                ExpressionKind::Double(value)
            }
            None => {
                if value_text.contains('.') {
                    let value = value_text.parse().unwrap();

                    ExpressionKind::InferredFloat(value)
                } else {
                    let value = match value_text.parse::<i32>() {
                        Ok(value) => value,
                        Err(error) => match error.kind() {
                            IntErrorKind::PosOverflow => {
                                ctx.add_error(
                                    text_range_to_span(value_token.text_range()),
                                    SemanticAnalysisError::ValueTooLarge(DataTypeKind::Integer),
                                );

                                i32::MAX
                            }
                            IntErrorKind::NegOverflow => {
                                ctx.add_error(
                                    text_range_to_span(value_token.text_range()),
                                    SemanticAnalysisError::ValueTooSmall(DataTypeKind::Integer),
                                );

                                i32::MIN
                            }
                            _ => unreachable!(),
                        },
                    };

                    ExpressionKind::InferredInteger(value)
                }
            }
        })
        .with_span(span),
    )
}
