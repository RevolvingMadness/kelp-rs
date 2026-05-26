use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTBreakStatement, lower_context::LowerContext, parser::Parser, span::span_of_cst_node,
    statement::expect_semicolon_ending, syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_break_statement(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::BreakStatement);

    parser.bump_str(SyntaxKind::BreakKeyword, "break");

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_break_statement(
    node: CSTBreakStatement,
    _ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = span_of_cst_node(&node);

    Some(ParsedStatementKind::Break.with_span(span))
}
