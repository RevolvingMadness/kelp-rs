use kelp_core::high::{item::ItemKind, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTMCFNDeclarationItem,
    expression::with_block::block::{lower_block_expression, try_parse_block_expression},
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    statement::try_parse_statement,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_mcfn_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::MCFNDeclarationItem);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");

    if !parser.expect_whitespace() || !try_parse_resource_location(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.expect_whitespace();

    if !try_parse_statement(parser) {
        parser.error("Expected statement");
    }

    parser.finish_node();

    true
}

#[must_use]
pub fn expect_mcfn_declaration_item_kind(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::MCFNDeclarationItem);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");

    parser.expect_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");

        return false;
    }

    parser.expect_whitespace();

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_mcfn_declaration_item_kind(
    node: CSTMCFNDeclarationItem,
    ctx: &mut SemanticAnalysisContext,
) -> Option<ItemKind> {
    let resource_location = lower_resource_location(node.resource_location()?)?;
    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(ItemKind::MCFNDeclaration(resource_location, body))
}
