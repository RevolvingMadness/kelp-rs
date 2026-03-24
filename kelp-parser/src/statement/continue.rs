use kelp_core::high::{
    semantic_analysis::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTContinueStatement, parser::Parser, span::span_of_cst_node,
    statement::expect_semicolon_ending, syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_continue_statement(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::ContinueStatement);

    parser.bump_str(SyntaxKind::ContinueKeyword, "continue");

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_continue_statement(
    node: CSTContinueStatement,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    Some(StatementKind::Continue.with_span(span))
}
