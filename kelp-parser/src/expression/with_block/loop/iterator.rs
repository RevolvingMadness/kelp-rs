use kelp_core::high::{
    expression::r#loop::{LoopExpression, LoopExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTIteratorLoopExpression,
    expression::{
        lower_expression, try_parse_expression,
        with_block::block::{lower_block_expression, try_parse_block_expression},
    },
    parser::Parser,
    pattern::{lower_pattern, try_parse_pattern},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_iterator_loop_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::IteratorLoopExpression);
    parser.bump_str(SyntaxKind::ForKeyword, "for");
    parser.skip_inline_whitespace();

    if !try_parse_pattern(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !parser.expect_str("in", SyntaxKind::InKeyword, "Expected 'in'") {
        parser.bump_until_whitespace();
    }

    parser.skip_whitespace();

    if !try_parse_expression(parser) {
        parser.error("Expected expression");
    }

    parser.skip_whitespace();

    if !try_parse_block_expression(parser) {
        parser.error("Expected body");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_iterator_loop_expression(
    node: CSTIteratorLoopExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<LoopExpression> {
    let span = span_of_cst_node(&node);

    let pattern = lower_pattern(node.pattern()?, ctx)?;

    let expression = lower_expression(node.expression()?, ctx)?;

    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(
        LoopExpressionKind::Iterator(false, pattern, Box::new(expression), Box::new(body))
            .with_span(span),
    )
}
