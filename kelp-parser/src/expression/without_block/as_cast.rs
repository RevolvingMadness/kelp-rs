use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTAsCastExpression, data_type::lower_data_type, expression::lower_expression,
    extension_traits::AstNodeExt as _, lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_as_cast_expression(
    node: CSTAsCastExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let expression = lower_expression(node.expression()?, ctx)?;
    let data_type = lower_data_type(node.data_type()?)?;

    Some(ParsedExpressionKind::AsCast(Box::new(expression), data_type).with_span(span))
}
