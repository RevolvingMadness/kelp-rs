use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{cst::CSTBooleanExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::unnecessary_wraps)]
pub fn lower_boolean_expression(
    node: CSTBooleanExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    Some(ctx.arena.allocate_expression(
        span,
        ParsedExpression::Boolean(node.true_keyword_token().is_some()),
    ))
}
