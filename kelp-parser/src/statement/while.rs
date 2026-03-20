use kelp_core::high::{
    semantic_analysis_context::SemanticAnalysisContext,
    statement::{Statement, StatementKind},
};

use crate::{
    cst::CSTWhileStatement,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_while_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::WhileStatement);
    parser.bump_str(SyntaxKind::WhileKeyword, "while");
    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_statement(parser) {
        parser.recover_newline("Expected statement");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_while_statement(
    node: CSTWhileStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let condition = lower_expression(node.condition()?, ctx)?;
    let body = lower_statement(node.body()?, ctx)?;

    Some(StatementKind::While(condition, Box::new(body)).with_span(span))
}
