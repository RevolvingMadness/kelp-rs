use std::num::IntErrorKind;

use kelp_core::{
    high::{
        expression::{Expression, ExpressionKind},
        semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
    },
    middle::data_type::DataType,
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
            Some(NumericKind::Byte) => match value_text.parse::<i8>() {
                Ok(value) => ExpressionKind::Byte(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooLarge(DataType::Byte),
                        );

                        ExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooSmall(DataType::Byte),
                        );

                        ExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Short) => match value_text.parse::<i16>() {
                Ok(value) => ExpressionKind::Short(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooLarge(DataType::Short),
                        );

                        ExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooSmall(DataType::Short),
                        );

                        ExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Integer) => match value_text.parse::<i32>() {
                Ok(value) => ExpressionKind::Integer(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooLarge(DataType::Integer),
                        );

                        ExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooSmall(DataType::Integer),
                        );

                        ExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
            Some(NumericKind::Long) => match value_text.parse::<i64>() {
                Ok(value) => ExpressionKind::Long(value),
                Err(error) => match error.kind() {
                    IntErrorKind::PosOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooLarge(DataType::Long),
                        );

                        ExpressionKind::Invalid
                    }
                    IntErrorKind::NegOverflow => {
                        ctx.add_error::<()>(
                            text_range_to_span(value_token.text_range()),
                            SemanticAnalysisError::ValueTooSmall(DataType::Long),
                        );

                        ExpressionKind::Invalid
                    }
                    _ => unreachable!(),
                },
            },
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
                    match value_text.parse::<i32>() {
                        Ok(value) => ExpressionKind::InferredInteger(value),
                        Err(error) => match error.kind() {
                            IntErrorKind::PosOverflow => {
                                ctx.add_error::<()>(
                                    text_range_to_span(value_token.text_range()),
                                    SemanticAnalysisError::ValueTooLarge(DataType::Integer),
                                );

                                ExpressionKind::Invalid
                            }
                            IntErrorKind::NegOverflow => {
                                ctx.add_error::<()>(
                                    text_range_to_span(value_token.text_range()),
                                    SemanticAnalysisError::ValueTooSmall(DataType::Integer),
                                );

                                ExpressionKind::Invalid
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
