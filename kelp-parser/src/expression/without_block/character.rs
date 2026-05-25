use kelp_core::high::expression::{Expression, ExpressionKind};

use crate::{cst::CSTCharacterExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_character_expression(
    node: CSTCharacterExpression,
    _ctx: &mut LowerContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let text_token = node.character_literal_token()?;
    let text = text_token.text().trim_matches('\'');

    Some(ExpressionKind::String(text.to_owned()).with_span(span))
}
