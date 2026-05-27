pub use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTDataType, CSTGenericNames, CSTTypeAliasDeclarationItem},
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTTypeAliasDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::TypeAliasDeclarationItem);
        parser.bump_str(SyntaxKind::TypeKeyword, "type");
        parser.skip_whitespace();

        if !parser.try_bump_identifier_kind(SyntaxKind::DataTypeName) {
            state.restore(parser);

            return false;
        }

        parser.skip_whitespace();

        if CSTGenericNames::try_parse(parser) {
            parser.skip_whitespace();
        }

        parser.expect_char('=', "Expected '='");

        parser.skip_whitespace();

        if !CSTDataType::try_parse(parser) {
            parser.error("Expected data type");
        }

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTTypeAliasDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();
        let generic_names = self.generic_names().and_then(|names| names.lower(ctx));
        let alias = self.data_type()?.lower(ctx)?;

        Some(ParsedItemKind::TypeAliasDeclaration {
            name_span,
            name: name.to_owned(),
            generic_names: generic_names.unwrap_or_default(),
            alias,
        })
    }
}
