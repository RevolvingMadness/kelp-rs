use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTIteratorLoopExpression, CSTPattern},
    expression::{
        lower_expression, try_parse_expression,
        with_block::block::{lower_block_expression, try_parse_block_expression},
    },
    extension_traits::AstNodeExt,
    extension_traits::ParsableAstNode,
    lower_context::LowerContext,
    parser::Parser,
    pattern::lower_pattern,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_iterator_loop_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::IteratorLoopExpression);
    parser.bump_str(SyntaxKind::ForKeyword, "for");
    parser.skip_inline_whitespace();

    if !CSTPattern::try_parse(parser) {
        state.restore(parser);

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
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let pattern = lower_pattern(node.pattern()?, ctx)?;

    let expression = lower_expression(node.expression()?, ctx)?;

    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(
        ParsedExpressionKind::ForLoop(false, pattern, Box::new(expression), Box::new(body))
            .with_span(span),
    )
}
