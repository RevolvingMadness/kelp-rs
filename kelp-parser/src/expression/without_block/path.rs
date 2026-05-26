use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{
    cst::CSTPathExpression, lower_context::LowerContext, path::generic::lower_generic_path,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_expression(
    node: CSTPathExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    Some(
        ctx.arena
            .allocate_expression(span, ParsedExpression::Path(path)),
    )
}
