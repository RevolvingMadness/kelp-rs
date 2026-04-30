use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTInfiniteLoopExpression,
    expression::with_block::block::{lower_block_expression, try_parse_block_expression},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_infinite_loop_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::InfiniteLoopExpression);
    parser.bump_str(SyntaxKind::LoopKeyword, "loop");
    parser.skip_inline_whitespace();

    if !try_parse_block_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_infinite_loop_expression(
    node: CSTInfiniteLoopExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(ExpressionKind::Loop(Box::new(body)).with_span(span))
}
