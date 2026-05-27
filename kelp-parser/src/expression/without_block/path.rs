use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTPathExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_expression(
    node: CSTPathExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let path = node.generic_path()?.lower(ctx)?;

    Some(ParsedExpressionKind::Path(path).with_span(node.span()))
}
