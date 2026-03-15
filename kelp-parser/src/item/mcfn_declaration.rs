use kelp_core::{
    high::item::{Item, ItemKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTMCFNDeclarationItem,
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::span_of_cst_node,
    statement::{lower_statement, try_parse_statement},
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_mcfn_declaration_item(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::MCFNDeclarationItem);
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
pub fn lower_mcfn_declaration_item(
    node: CSTMCFNDeclarationItem,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Item> {
    let span = span_of_cst_node(&node);

    let resource_location = lower_resource_location(node.resource_location()?)?;
    let body = lower_statement(node.statement()?, ctx)?;

    Some(ItemKind::MCFNDeclaration(resource_location, Box::new(body)).with_span(span))
}
