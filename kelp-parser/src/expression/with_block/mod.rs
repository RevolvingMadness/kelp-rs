use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTExpressionWithBlock,
    expression::with_block::{
        block::lower_block_expression, r#if::lower_if_expression, r#loop::lower_loop_expression,
    },
    extension_traits::AstNodeExt,
    lower_context::LowerContext,
};

pub mod block;
pub mod r#if;
pub mod r#loop;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_with_block(
    node: CSTExpressionWithBlock,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    match node {
        CSTExpressionWithBlock::BlockExpression(node) => {
            let span = node.span();

            let expression = lower_block_expression(node, ctx)?;

            Some(ParsedExpressionKind::Block(expression).with_span(span))
        }
        CSTExpressionWithBlock::IfExpression(node) => lower_if_expression(node, ctx),
        CSTExpressionWithBlock::LoopExpression(node) => lower_loop_expression(node, ctx),
    }
}
