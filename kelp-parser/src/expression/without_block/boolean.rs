use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{cst::CSTBooleanExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::unnecessary_wraps)]
pub fn lower_boolean_expression(
    node: CSTBooleanExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Boolean(node.true_keyword_token().is_some()).with_span(span))
}
