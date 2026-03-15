use kelp_core::{
    high::statement::{Statement, StatementKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTLoopStatement,
    parser::Parser,
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_loop_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::LoopStatement);
    parser.bump_str(SyntaxKind::LoopKeyword, "loop");
    parser.skip_inline_whitespace();

    if !try_parse_statement(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_loop_statement(
    node: CSTLoopStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let body = lower_statement(node.body()?, ctx)?;

    Some(StatementKind::Loop(Box::new(body)).with_span(span))
}
