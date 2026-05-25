use kelp_core::high::expression::{Expression, ExpressionKind};

use crate::{cst::CSTUnderscoreExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_underscore_expression(
    node: CSTUnderscoreExpression,
    _ctx: &mut LowerContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Underscore.with_span(span))
}
