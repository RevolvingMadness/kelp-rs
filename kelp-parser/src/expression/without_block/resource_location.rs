use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTResourceLocationExpression, resource_location::lower_resource_location,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location_expression(
    node: CSTResourceLocationExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    let span = span_of_cst_node(&node);

    Some(ExpressionKind::ResourceLocation(Box::new(resource_location)).with_span(span))
}
