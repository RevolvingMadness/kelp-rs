use kelp_core::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTIfStatement,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    statement::{block::try_parse_block_statement, lower_statement},
    syntax::SyntaxKind,
};

pub fn try_parse_if_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::IfStatement);
    parser.bump_str(SyntaxKind::IfKeyword, "if");
    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_block_statement(parser) {
        parser.recover_newline("Expected block statement");
    }

    parser.skip_inline_whitespace();

    if parser.peek_identifier() == Some("else") {
        parser.bump_str(SyntaxKind::ElseKeyword, "else");

        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('{') {
            if !try_parse_block_statement(parser) {
                parser.recover_newline("Expected block statement");
            }
        } else if parser.peek_identifier() == Some("if") {
            if !try_parse_if_statement(parser) {
                parser.recover_newline("Expected if statement");
            }
        } else {
            parser.recover_newline("Expected block or if statement");
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_if_statement(
    node: CSTIfStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let condition = lower_expression(node.condition()?, ctx)?;
    let body = lower_statement(node.body()?, ctx)?;
    let else_body = node
        .else_body()
        .and_then(|statement| lower_statement(statement, ctx))
        .map(Box::new);

    Some(StatementKind::If(condition, Box::new(body), else_body).with_span(span))
}
