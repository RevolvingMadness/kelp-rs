use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{cst::CSTStringExpression, extension_traits::AstNodeExt, lower_context::LowerContext};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_string_expression(
    node: CSTStringExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let text_token = node.string_literal_token()?;
    let string = text_token.text().trim_matches('"');

    Some(ParsedExpressionKind::String(string.to_owned()).with_span(span))
}
