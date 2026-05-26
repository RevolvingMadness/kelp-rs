use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::CSTIndexExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_index_expression(
    node: CSTIndexExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let mut expressions = node.expressions();

    let expression = lower_expression(expressions.next()?, ctx)?;
    let index = lower_expression(expressions.next()?, ctx)?;

    Some(
        ctx.arena
            .allocate_expression(span, ParsedExpression::Index(expression, index)),
    )
}
