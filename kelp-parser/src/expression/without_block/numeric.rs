use std::num::IntErrorKind;

use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};
use ordered_float::NotNan;

use crate::{
    cst::CSTNumericExpression,
    extension_traits::{AstNodeExt, LowerableAstNode, SyntaxTokenExt as _},
    lower_context::{LowerContext, LowerDataType, LowerError},
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

impl LowerableAstNode for CSTNumericExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let value_token = self.fractional_value_token()?;
        let value_text = value_token.text();

        let suffix = self
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
                                value_token.span(),
                                LowerError::ValueTooBig(LowerDataType::Byte),
                            );

                            ParsedExpressionKind::Invalid
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error_unit(
                                value_token.span(),
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
                                value_token.span(),
                                LowerError::ValueTooBig(LowerDataType::Short),
                            );

                            ParsedExpressionKind::Invalid
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error_unit(
                                value_token.span(),
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
                                value_token.span(),
                                LowerError::ValueTooBig(LowerDataType::Integer),
                            );

                            ParsedExpressionKind::Invalid
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error_unit(
                                value_token.span(),
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
                                value_token.span(),
                                LowerError::ValueTooBig(LowerDataType::Long),
                            );

                            ParsedExpressionKind::Invalid
                        }
                        IntErrorKind::NegOverflow => {
                            ctx.add_error_unit(
                                value_token.span(),
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
                                        value_token.span(),
                                        LowerError::ValueTooBig(LowerDataType::Integer),
                                    );

                                    ParsedExpressionKind::Invalid
                                }
                                IntErrorKind::NegOverflow => {
                                    ctx.add_error_unit(
                                        value_token.span(),
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
            .with_span(self.span()),
        )
    }
}
