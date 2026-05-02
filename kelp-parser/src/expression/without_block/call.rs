use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{cst::CSTCallExpression, expression::lower_expression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_call_expression(
    node: CSTCallExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let callee = lower_expression(node.callee()?, ctx)?;

    let arguments = node
        .arguments()
        .filter_map(|argument| lower_expression(argument, ctx))
        .collect();

    Some(
        ExpressionKind::Call {
            callee: Box::new(callee),
            arguments,
        }
        .with_span(span),
    )
}
