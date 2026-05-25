use kelp_core::high::expression::{Expression, ExpressionKind};

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
) -> Option<Expression> {
    let expression = lower_expression(node.expression()?, ctx)?;
    let field_token = node.field_name_token()?;

    let field_span = text_range_to_span(field_token.text_range());
    let field = field_token.text().to_owned();

    let span = span_of_cst_node(&node);

    Some(ExpressionKind::FieldAccess(Box::new(expression), field_span, field).with_span(span))
}
