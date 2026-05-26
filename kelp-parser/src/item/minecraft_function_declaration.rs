use kelp_core::parsed::item::ParsedItemKind;
use rowan::ast::AstNode;

use crate::{
    cst::CSTMinecraftFunctionDeclarationItem,
    expression::with_block::block::{lower_block_expression, try_parse_block_expression},
    expression_sigil::assert_not_sigil,
    lower_context::LowerContext,
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::text_range_to_span,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_minecraft_function_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::MinecraftFunctionDeclarationItem);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");

    if !parser.expect_whitespace() || !try_parse_resource_location(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.expect_whitespace();

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();

    true
}

pub fn expect_minecraft_function_declaration_item_kind(parser: &mut Parser) {
    parser.start_node(SyntaxKind::MinecraftFunctionDeclarationItem);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");

    let mut succeeded = parser.expect_whitespace();

    if !try_parse_resource_location(parser) && succeeded {
        parser.error("Expected resource location");
    }

    succeeded = parser.expect_whitespace() || succeeded;

    if !try_parse_block_expression(parser) && succeeded {
        parser.error("Expected block expression");
    }

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_minecraft_function_declaration_item_kind(
    node: CSTMinecraftFunctionDeclarationItem,
    ctx: &mut LowerContext,
) -> Option<ParsedItemKind> {
    let resource_location_token = node.resource_location()?;
    let resource_location_span = text_range_to_span(resource_location_token.syntax().text_range());
    let resource_location = lower_resource_location(resource_location_token, ctx)?;
    let resource_location = assert_not_sigil(resource_location, resource_location_span, ctx)?;
    let body = lower_block_expression(node.block_expression()?, ctx)?;

    Some(ParsedItemKind::MinecraftFunctionDeclaration {
        resource_location,
        body,
    })
}
