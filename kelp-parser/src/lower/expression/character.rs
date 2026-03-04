use kelp_core::{
    expression::{
        Expression, ExpressionKind, constant::ConstantExpressionKind,
        literal::LiteralExpressionKind,
    },
    high::snbt_string::HighSNBTString,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{cst::CSTCharacterExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_character_expression(node: CSTCharacterExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let text_token = node.character_literal_token()?;
    let text = text_token.text().trim_matches('\'');

    Some(
        ExpressionKind::Constant(
            ConstantExpressionKind::Literal(
                LiteralExpressionKind::String(HighSNBTString {
                    span,
                    snbt_string: SNBTString(false, text.to_string()),
                })
                .with_span(span),
            )
            .with_span(span),
        )
        .with_span(span),
    )
}
