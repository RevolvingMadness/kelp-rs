use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::CSTTupleExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_expression(
    node: CSTTupleExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let expressions = node
        .expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect();

    Some(
        ctx.arena
            .allocate_expression(span, ParsedExpression::Tuple(expressions)),
    )
}
