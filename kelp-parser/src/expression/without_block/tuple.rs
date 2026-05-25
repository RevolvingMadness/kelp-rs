use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTTupleExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_expression(
    node: CSTTupleExpression,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let span = span_of_cst_node(&node);

    let expressions = node
        .expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect();

    Some(
        ctx.allocator
            .allocate_expression(span, Expression::Tuple(expressions)),
    )
}
