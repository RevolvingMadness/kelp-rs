use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTIndexExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_index_expression(
    node: CSTIndexExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let mut expressions = node.expressions();

    let expression = expressions.next()?.lower(ctx)?;
    let index = expressions.next()?.lower(ctx)?;

    Some(ParsedExpressionKind::Index(Box::new(expression), Box::new(index)).with_span(span))
}
