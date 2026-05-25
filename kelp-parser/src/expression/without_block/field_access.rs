use kelp_core::high::expression::{Expression, ExpressionId};

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
) -> Option<ExpressionId> {
    let expression = lower_expression(node.expression()?, ctx)?;
    let field_token = node.field_name_token()?;

    let field_span = text_range_to_span(field_token.text_range());
    let field = field_token.text().to_owned();

    let span = span_of_cst_node(&node);

    Some(
        ctx.allocator
            .allocate_expression(span, Expression::FieldAccess(expression, field_span, field)),
    )
}
