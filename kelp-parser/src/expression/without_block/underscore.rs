use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{cst::CSTUnderscoreExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_underscore_expression(
    node: CSTUnderscoreExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = span_of_cst_node(&node);

    Some(ParsedExpressionKind::Underscore.with_span(span))
}
