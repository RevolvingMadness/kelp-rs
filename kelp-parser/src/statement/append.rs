use kelp_core::high::{semantic_analysis_context::SemanticAnalysisContext, statement::{Statement, StatementKind}};

use crate::{
    cst::CSTAppendStatement,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_append_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::AppendStatement);

    parser.bump_str(SyntaxKind::AppendKeyword, "append");

    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if !try_parse_expression(parser) {
        parser.error("Expected expression");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_append_statement(
    node: CSTAppendStatement,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let target = lower_expression(node.target()?, ctx)?;
    let value = lower_expression(node.value()?, ctx)?;

    Some(StatementKind::Append(target, Box::new(value)).with_span(span))
}
