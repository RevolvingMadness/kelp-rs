use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{cst::CSTIndexExpression, expression::lower_expression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_index_expression(
    node: CSTIndexExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let mut expressions = node.expressions();

    let expression = lower_expression(expressions.next()?, ctx)?;
    let index = lower_expression(expressions.next()?, ctx)?;

    Some(ExpressionKind::Index(Box::new(expression), Box::new(index)).with_span(span))
}
