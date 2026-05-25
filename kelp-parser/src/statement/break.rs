use kelp_core::high::statement::{Statement, StatementId};

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
    ctx: &mut LowerContext,
) -> Option<StatementId> {
    let span = span_of_cst_node(&node);

    Some(ctx.allocator.allocate_statement(span, Statement::Break))
}
