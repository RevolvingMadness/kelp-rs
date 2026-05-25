use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTIndexExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_index_expression(
    node: CSTIndexExpression,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let span = span_of_cst_node(&node);

    let mut expressions = node.expressions();

    let expression = lower_expression(expressions.next()?, ctx)?;
    let index = lower_expression(expressions.next()?, ctx)?;

    Some(
        ctx.allocator
            .allocate_expression(span, Expression::Index(expression, index)),
    )
}
