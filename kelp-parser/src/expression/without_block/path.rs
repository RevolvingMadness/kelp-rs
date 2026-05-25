use kelp_core::high::expression::{Expression, ExpressionKind};

use crate::{
    cst::CSTPathExpression, lower_context::LowerContext, path::generic::lower_generic_path,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_expression(
    node: CSTPathExpression,
    _ctx: &mut LowerContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    Some(ExpressionKind::Path(path).with_span(span))
}
