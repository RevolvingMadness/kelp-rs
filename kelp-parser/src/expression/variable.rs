use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{cst::CSTVariableExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_variable_expression(
    node: CSTVariableExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let name_token = node.identifier_token()?;
    let name = name_token.text();

    Some(ExpressionKind::Variable(name.to_string()).with_span(span))
}
