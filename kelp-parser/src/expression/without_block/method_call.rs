use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTMethodCallExpression,
    expression::{lower_expression, without_block::call::lower_call_arguments},
    lower_context::LowerContext,
    path::generic::lower_generic_path_segment,
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_method_call_expression(
    node: CSTMethodCallExpression,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let receiver = lower_expression(node.expression()?, ctx)?;

    let callee = lower_generic_path_segment(node.generic_path_segment()?)?;

    let arguments = node
        .call_arguments()
        .map(|arguments| lower_call_arguments(arguments, ctx));

    let span = span_of_cst_node(&node);

    Some(ctx.allocator.allocate_expression(
        span,
        Expression::MethodCall {
            receiver,
            callee,
            arguments: arguments.unwrap_or_default(),
        },
    ))
}
