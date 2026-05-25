use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTExpressionWithBlock,
    expression::with_block::{
        block::lower_block_expression, r#if::lower_if_expression, r#loop::lower_loop_expression,
    },
    lower_context::LowerContext,
    span::span_of_cst_node,
};

pub mod block;
pub mod r#if;
pub mod r#loop;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_with_block(
    node: CSTExpressionWithBlock,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    match node {
        CSTExpressionWithBlock::BlockExpression(node) => {
            let span = span_of_cst_node(&node);

            let expression = lower_block_expression(node, ctx)?;

            Some(
                ctx.allocator
                    .allocate_expression(span, Expression::Block(expression)),
            )
        }
        CSTExpressionWithBlock::IfExpression(node) => lower_if_expression(node, ctx),
        CSTExpressionWithBlock::LoopExpression(node) => lower_loop_expression(node, ctx),
    }
}
