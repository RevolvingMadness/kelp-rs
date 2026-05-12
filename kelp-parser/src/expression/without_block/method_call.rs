use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTMethodCallExpression,
    expression::{lower_expression, without_block::call::lower_call_arguments},
    path::generic::lower_generic_path_segment,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_method_call_expression(
    node: CSTMethodCallExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let receiver = lower_expression(node.expression()?, ctx)?;

    let callee = lower_generic_path_segment(node.generic_path_segment()?)?;

    let arguments = lower_call_arguments(node.call_arguments()?, ctx);

    let span = span_of_cst_node(&node);

    Some(
        ExpressionKind::MethodCall {
            receiver: Box::new(receiver),
            callee,
            arguments,
        }
        .with_span(span),
    )
}
