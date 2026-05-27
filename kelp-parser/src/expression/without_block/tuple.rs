use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTTupleExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_expression(
    node: CSTTupleExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let expressions = node
        .expressions()
        .filter_map(|expression| expression.lower(ctx))
        .collect();

    Some(ParsedExpressionKind::Tuple(expressions).with_span(span))
}
