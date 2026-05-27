use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTTupleExpression, expression::lower_expression, extension_traits::AstNodeExt as _,
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
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect();

    Some(ParsedExpressionKind::Tuple(expressions).with_span(span))
}
