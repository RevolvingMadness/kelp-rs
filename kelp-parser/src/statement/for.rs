use kelp_core::high::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTForStatement,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    pattern::{lower_pattern, try_parse_pattern},
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_for_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ForStatement);
    parser.bump_str(SyntaxKind::ForKeyword, "for");
    parser.skip_inline_whitespace();

    if !try_parse_pattern(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !parser.expect_str("in", SyntaxKind::InKeyword, "Expected 'in'") {
        parser.bump_until_whitespace();
    }

    parser.skip_whitespace();

    if !try_parse_expression(parser) {
        parser.error("Expected expression");
    }

    parser.skip_whitespace();

    if !try_parse_statement(parser) {
        parser.error("Expected statement");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_for_statement(
    node: CSTForStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let pattern = lower_pattern(node.pattern()?, ctx)?;

    let expression = lower_expression(node.expression()?, ctx)?;

    let statement = lower_statement(node.statement()?, ctx)?;

    Some(StatementKind::For(false, pattern, expression, Box::new(statement)).with_span(span))
}
