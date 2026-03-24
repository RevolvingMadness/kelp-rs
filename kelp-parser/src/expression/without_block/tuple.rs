use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{cst::CSTTupleExpression, expression::lower_expression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_expression(
    node: CSTTupleExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expressions = node
        .expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect();

    Some(ExpressionKind::Tuple(expressions).with_span(span))
}
