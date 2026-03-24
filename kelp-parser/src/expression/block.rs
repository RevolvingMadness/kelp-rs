use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTBlockExpression,
    parser::Parser,
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

pub fn try_parse_block_expression(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_char('{') {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::BlockExpression);

    let mut is_first = true;

    parser.skip_whitespace();

    loop {
        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        if !is_first && !parser.try_parse_newline_whitespace() {
            parser.recover_newline("Expected newline to mark end of statement");

            continue;
        }

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        if !try_parse_statement(parser) {
            parser.recover_newline("Expected statement");
        }

        is_first = false;
    }

    parser.skip_whitespace();

    parser.expect_char('}', "Expected '}'");
    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_block_expression(
    node: CSTBlockExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let statements = node
        .statements()
        .filter_map(|statment| lower_statement(statment, ctx))
        .collect();

    Some(ExpressionKind::Block(statements).with_span(span))
}
