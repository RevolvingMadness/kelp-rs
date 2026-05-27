use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTRemoveStatement,
    expression::{lower_expression, try_parse_expression},
    extension_traits::AstNodeExt,
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_remove_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::RemoveStatement);

    parser.bump_str(SyntaxKind::RemoveKeyword, "remove");

    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        state.restore(parser);

        return false;
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_remove_statement(
    node: CSTRemoveStatement,
    ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = node.span();

    let target = lower_expression(node.target()?, ctx)?;

    Some(ParsedStatementKind::Remove(target).with_span(span))
}
