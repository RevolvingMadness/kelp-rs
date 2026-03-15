use kelp_core::{
    high::statement::{Statement, StatementKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTBlockStatement,
    parser::Parser,
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

pub fn try_parse_block_statement(parser: &mut Parser) -> bool {
    if !parser.expect_no_bump('{', "Expected block statement") {
        return false;
    }

    parser.start_node(SyntaxKind::BlockStatement);
    parser.bump_char();

    let mut is_first = true;

    parser.skip_whitespace();

    loop {
        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        if !is_first && !parser.try_parse_newline_whitespace() {
            parser.recover_newline("Expected newline to mark end of statement");

            is_first = false;

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
pub fn lower_block_statement(
    node: CSTBlockStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let statements = node
        .statements()
        .filter_map(|statment| lower_statement(statment, ctx))
        .collect();

    Some(StatementKind::Block(statements).with_span(span))
}
