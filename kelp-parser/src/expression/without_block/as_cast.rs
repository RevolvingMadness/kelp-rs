use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTAsCastExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_as_cast_expression(
    node: CSTAsCastExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let expression = node.expression()?.lower(ctx)?;
    let data_type = node.data_type()?.lower(ctx)?;

    Some(ParsedExpressionKind::AsCast(Box::new(expression), data_type).with_span(span))
}
