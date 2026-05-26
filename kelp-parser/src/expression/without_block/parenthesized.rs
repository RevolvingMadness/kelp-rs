use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::CSTParenthesizedExpression, expression::lower_expression, lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_parenthesized_expression(
    node: CSTParenthesizedExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(expression)
}
