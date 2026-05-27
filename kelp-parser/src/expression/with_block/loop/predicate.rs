use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTPredicateLoopExpression,
    expression::{
        lower_expression, try_parse_expression,
        with_block::block::{lower_block_expression, try_parse_block_expression},
    },
    extension_traits::AstNodeExt,
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_predicate_loop_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::PredicateLoopExpression);
    parser.bump_str(SyntaxKind::WhileKeyword, "while");
    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        state.restore(parser);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_block_expression(parser) {
        parser.recover_not_whitespace("Expected statement");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_predicate_loop_expression(
    node: CSTPredicateLoopExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let condition = lower_expression(node.expression()?, ctx)?;
    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(ParsedExpressionKind::WhileLoop(Box::new(condition), Box::new(body)).with_span(span))
}
