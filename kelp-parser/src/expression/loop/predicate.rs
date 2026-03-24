use kelp_core::high::{
    expression::r#loop::{LoopExpression, LoopExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTPredicateLoopExpression,
    expression::{
        block::{lower_block_expression, try_parse_block_expression},
        lower_expression, try_parse_expression,
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_predicate_loop_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::PredicateLoopExpression);
    parser.bump_str(SyntaxKind::WhileKeyword, "while");
    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_block_expression(parser) {
        parser.recover_newline("Expected statement");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_predicate_loop_expression(
    node: CSTPredicateLoopExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<LoopExpression> {
    let span = span_of_cst_node(&node);

    let condition = lower_expression(node.expression()?, ctx)?;
    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(LoopExpressionKind::Predicate(Box::new(condition), Box::new(body)).with_span(span))
}
