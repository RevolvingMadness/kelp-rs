use kelp_core::{
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{cst::CSTUnitExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unit_expression(
    node: CSTUnitExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Unit.with_span(span))
}
