use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTFieldAccessExpression,
    expression::lower_expression,
    extension_traits::{AstNodeExt, SyntaxTokenExt},
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_field_access_expression(
    node: CSTFieldAccessExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let expression = lower_expression(node.expression()?, ctx)?;
    let field_token = node.field_name_token()?;

    let field_span = field_token.span();
    let field = field_token.text().to_owned();

    let span = node.span();

    Some(ParsedExpressionKind::FieldAccess(Box::new(expression), field_span, field).with_span(span))
}
