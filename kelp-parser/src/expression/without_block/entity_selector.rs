use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTEntitySelectorExpression, entity_selector::lower_entity_selector,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_entity_selector_expression(
    node: CSTEntitySelectorExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let entity_selector = lower_entity_selector(node.entity_selector()?, ctx)?;

    let span = span_of_cst_node(&node);

    Some(ExpressionKind::EntitySelector(Box::new(entity_selector)).with_span(span))
}
