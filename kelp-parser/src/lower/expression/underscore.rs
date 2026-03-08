use kelp_core::expression::{Expression, ExpressionKind};

use crate::{cst::CSTUnderscoreExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_underscore_expression(node: CSTUnderscoreExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Underscore.with_span(span))
}
