use kelp_core::high::expression::ExpressionId;

use crate::{
    cst::CSTParenthesizedExpression, expression::lower_expression, lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_parenthesized_expression(
    node: CSTParenthesizedExpression,
    ctx: &mut LowerContext,
) -> Option<ExpressionId> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(expression)
}
