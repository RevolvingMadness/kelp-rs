use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTBlockExpression, CSTMinecraftFunctionDeclarationItem, CSTResourceLocation},
    expression_sigil::assert_not_sigil,
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_minecraft_function_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::MinecraftFunctionDeclarationItem);
    parser.bump_str(SyntaxKind::MCFNKeyword, "mcfn");

    if !parser.expect_whitespace() || !CSTResourceLocation::try_parse(parser) {
        state.restore(parser);

        return false;
    }

    parser.expect_whitespace();

    CSTBlockExpression::expect(parser, "Expected mcfunction body");

    parser.finish_node();

    true
}

impl LowerableAstNode for CSTMinecraftFunctionDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let resource_location = self.resource_location()?;
        let resource_location_span = resource_location.span();
        let resource_location = resource_location.lower(ctx)?;
        let resource_location = assert_not_sigil(resource_location, resource_location_span, ctx)?;

        let body = self.block_expression()?.lower(ctx)?;

        Some(ParsedItemKind::MinecraftFunctionDeclaration {
            resource_location,
            body: Box::new(body),
        })
    }
}
