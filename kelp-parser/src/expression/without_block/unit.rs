use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{cst::CSTUnitExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unit_expression(
    node: CSTUnitExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    Some(ctx.arena.allocate_expression(span, ParsedExpression::Unit))
}
