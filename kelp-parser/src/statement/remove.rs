use kelp_core::high::{
    semantic_analysis::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTRemoveStatement,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
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
        parser.restore_state(state);

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
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let target = lower_expression(node.target()?, ctx)?;

    Some(StatementKind::Remove(target).with_span(span))
}
