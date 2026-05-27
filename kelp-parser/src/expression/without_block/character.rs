use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTCharacterExpression, extension_traits::AstNodeExt, lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_character_expression(
    node: CSTCharacterExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let text_token = node.character_literal_token()?;
    let text = text_token.text().trim_matches('\'');

    Some(ParsedExpressionKind::String(text.to_owned()).with_span(span))
}
