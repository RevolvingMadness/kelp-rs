use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{cst::CSTStringExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_string_expression(
    node: CSTStringExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let text_token = node.string_literal_token()?;
    let string = text_token.text().trim_matches('"');

    Some(ExpressionKind::String(string.to_owned()).with_span(span))
}
