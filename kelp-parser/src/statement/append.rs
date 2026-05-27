use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::{CSTAppendStatement, CSTExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_append_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::AppendStatement);

    parser.bump_str(SyntaxKind::AppendKeyword, "append");

    parser.skip_inline_whitespace();

    if !CSTExpression::try_parse(parser) {
        state.restore(parser);

        return false;
    }

    parser.skip_inline_whitespace();

    if !CSTExpression::try_parse(parser) {
        parser.error("Expected expression");
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_append_statement(
    node: CSTAppendStatement,
    ctx: &mut LowerContext,
) -> Option<ParsedStatement> {
    let span = node.span();

    let target = node.target()?.lower(ctx)?;
    let value = node.value()?.lower(ctx)?;

    Some(ParsedStatementKind::Append(target, Box::new(value)).with_span(span))
}
