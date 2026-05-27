use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{cst::CSTUnitExpression, extension_traits::AstNodeExt, lower_context::LowerContext};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unit_expression(
    node: CSTUnitExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    Some(ParsedExpressionKind::Unit.with_span(span))
}
