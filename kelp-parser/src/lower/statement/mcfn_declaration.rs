use kelp_core::statement::{Statement, StatementKind};

use crate::{
    cst::CSTMCFNDeclarationStatement,
    lower::{
        resource_location::{lower_resource_location, try_parse_resource_location},
        statement::{lower_statement, try_parse_statement},
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_mcfn_declaration_statement(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::MCFNDeclarationStatement);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");
    parser.expect_inline_whitespace();

    if !try_parse_resource_location(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();
    if !try_parse_statement(parser) {
        parser.error("Expected statement");
    }
    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_mcfn_declaration_statement(node: CSTMCFNDeclarationStatement) -> Option<Statement> {
    let span = span_of_cst_node(&node);

    let resource_location = lower_resource_location(node.resource_location()?)?;
    let body = lower_statement(node.statement()?)?;

    Some(StatementKind::MCFNDeclaration(resource_location, Box::new(body)).with_span(span))
}
