use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTPathExpression, extension_traits::AstNodeExt as _, lower_context::LowerContext,
    path::generic::lower_generic_path,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_expression(
    node: CSTPathExpression,
    _ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let path = lower_generic_path(node.generic_path()?)?;

    Some(ParsedExpressionKind::Path(path).with_span(span))
}
