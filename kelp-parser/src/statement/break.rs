use kelp_core::high::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{cst::CSTBreakStatement, parser::Parser, span::span_of_cst_node, syntax::SyntaxKind};

#[must_use]
pub fn try_parse_break_statement(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::BreakStatement);

    parser.bump_str(SyntaxKind::BreakKeyword, "break");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_break_statement(
    node: CSTBreakStatement,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    Some(StatementKind::Break.with_span(span))
}
