use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTAsCastExpression, data_type::lower_data_type, expression::lower_expression,
    lower_context::LowerContext, span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_as_cast_expression(
    node: CSTAsCastExpression,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?, ctx)?;
    let data_type = lower_data_type(node.data_type()?)?;

    Some(
        ctx.allocator
            .allocate_expression(span, Expression::AsCast(expression, data_type)),
    )
}
