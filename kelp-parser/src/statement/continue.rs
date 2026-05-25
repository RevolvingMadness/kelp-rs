use kelp_core::high::statement::Statement;
use la_arena::Idx;

use crate::{
    cst::CSTContinueStatement, lower_context::LowerContext, parser::Parser, span::span_of_cst_node,
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
    ctx: &mut LowerContext,
) -> Option<Idx<Statement>> {
    let span = span_of_cst_node(&node);

    Some(ctx.allocator.allocate_statement(span, Statement::Continue))
}
