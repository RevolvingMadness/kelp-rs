use kelp_core::expression::{Expression, ExpressionKind};

use crate::{cst::CSTNumericExpression, span::span_of_cst_node};

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
pub fn lower_numeric_expression(node: CSTNumericExpression) -> Option<Expression> {
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
                let value = value_text.parse().ok()?;

                ExpressionKind::Byte(value)
            }
            Some(NumericKind::Short) => {
                let value = value_text.parse().ok()?;

                ExpressionKind::Short(value)
            }
            Some(NumericKind::Integer) => {
                let value = value_text.parse().ok()?;

                ExpressionKind::Integer(value)
            }
            Some(NumericKind::Long) => {
                let value = value_text.parse().ok()?;

                ExpressionKind::Long(value)
            }
            Some(NumericKind::Float) => {
                let value = value_text.parse().ok()?;

                ExpressionKind::Float(value)
            }
            Some(NumericKind::Double) => {
                let value = value_text.parse().ok()?;

                ExpressionKind::Double(value)
            }
            None => {
                if value_text.contains('.') {
                    let value = value_text.parse().ok()?;

                    ExpressionKind::InferredFloat(value)
                } else {
                    let value = value_text.parse().ok()?;

                    ExpressionKind::InferredInteger(value)
                }
            }
        })
        .with_span(span),
    )
}
