use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTUnderscoreExpression, extension_traits::AstNodeExt as _, lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_underscore_expression(
    node: CSTUnderscoreExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    Some(ParsedExpressionKind::Underscore.with_span(span))
}
