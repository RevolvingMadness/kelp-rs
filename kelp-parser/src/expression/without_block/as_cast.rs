use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTAsCastExpression, data_type::lower_data_type, expression::lower_expression,
    lower_context::LowerContext, span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_as_cast_expression(
    node: CSTAsCastExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?, ctx)?;
    let data_type = lower_data_type(node.data_type()?)?;

    Some(ParsedExpressionKind::AsCast(Box::new(expression), data_type).with_span(span))
}
