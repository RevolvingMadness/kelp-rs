use kelp_core::expression::{Expression, ExpressionKind};

use crate::{cst::CSTTupleExpression, lower::expression::lower_expression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_expression(node: CSTTupleExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expressions = node.expressions().filter_map(lower_expression).collect();

    Some(ExpressionKind::Tuple(expressions).with_span(span))
}
