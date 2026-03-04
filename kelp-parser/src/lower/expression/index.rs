use kelp_core::expression::{Expression, ExpressionKind};

use crate::{cst::CSTIndexExpression, lower::expression::lower_expression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_index_expression(node: CSTIndexExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let mut expressions = node.expressions();

    let expression = lower_expression(expressions.next()?)?;
    let index = lower_expression(expressions.next()?)?;

    Some(ExpressionKind::Index(Box::new(expression), Box::new(index)).with_span(span))
}
