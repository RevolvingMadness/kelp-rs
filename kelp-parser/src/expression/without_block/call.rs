use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::{CSTCallArguments, CSTCallExpression},
    expression::lower_expression,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_arguments(
    node: CSTCallArguments,
    ctx: &mut SemanticAnalysisContext,
) -> Vec<Expression> {
    node.expressions()
        .filter_map(|expression| lower_expression(expression, ctx))
        .collect()
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_expression(
    node: CSTCallExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let callee = lower_expression(node.callee()?, ctx)?;

    let arguments = node
        .call_arguments()
        .map(|arguments| lower_call_arguments(arguments, ctx));

    Some(
        ExpressionKind::Call {
            callee: Box::new(callee),
            arguments: arguments.unwrap_or_default(),
        }
        .with_span(span),
    )
}
