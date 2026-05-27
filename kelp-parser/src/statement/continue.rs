use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTContinueStatement, extension_traits::AstNodeExt, lower_context::LowerContext,
    parser::Parser, statement::expect_semicolon_ending, syntax::SyntaxKind,
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
    _ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = node.span();

    Some(ParsedStatementKind::Continue.with_span(span))
}
