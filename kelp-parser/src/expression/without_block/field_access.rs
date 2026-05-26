use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::CSTFieldAccessExpression,
    expression::lower_expression,
    lower_context::LowerContext,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_field_access_expression(
    node: CSTFieldAccessExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let expression = lower_expression(node.expression()?, ctx)?;
    let field_token = node.field_name_token()?;

    let field_span = text_range_to_span(field_token.text_range());
    let field = field_token.text().to_owned();

    let span = span_of_cst_node(&node);

    Some(ctx.arena.allocate_expression(
        span,
        ParsedExpression::FieldAccess(expression, field_span, field),
    ))
}
