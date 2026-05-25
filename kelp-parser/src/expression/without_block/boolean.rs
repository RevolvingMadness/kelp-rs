use kelp_core::high::expression::{Expression, ExpressionId};

use crate::{cst::CSTBooleanExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::unnecessary_wraps)]
pub fn lower_boolean_expression(
    node: CSTBooleanExpression,
    ctx: &mut LowerContext,
) -> Option<ExpressionId> {
    let span = span_of_cst_node(&node);

    Some(ctx.allocator.allocate_expression(
        span,
        Expression::Boolean(node.true_keyword_token().is_some()),
    ))
}
