use kelp_core::expression::{Expression, ExpressionKind, constant::ConstantExpressionKind};

use crate::{cst::CSTUnitExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unit_expression(node: CSTUnitExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Constant(ConstantExpressionKind::Unit.with_span(span)).with_span(span))
}
