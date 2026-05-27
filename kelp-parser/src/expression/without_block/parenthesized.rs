use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::CSTParenthesizedExpression, extension_traits::LowerableAstNode,
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_parenthesized_expression(
    node: CSTParenthesizedExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let expression = node.expression()?.lower(ctx)?;

    Some(expression)
}
