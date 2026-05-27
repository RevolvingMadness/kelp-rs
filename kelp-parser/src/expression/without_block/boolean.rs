use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{cst::CSTBooleanExpression, extension_traits::AstNodeExt, lower_context::LowerContext};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::unnecessary_wraps)]
pub fn lower_boolean_expression(
    node: CSTBooleanExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    Some(ParsedExpressionKind::Boolean(node.true_token().is_some()).with_span(span))
}
