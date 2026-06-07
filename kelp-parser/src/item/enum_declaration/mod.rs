use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTEnumDeclarationItem, CSTEnumVariants},
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod variant;
pub mod variants;

impl ParsableAstNode for CSTEnumDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_identifier() != Some("enum") {
            return false;
        }

        parser.start_node(SyntaxKind::EnumDeclarationItem);

        parser.bump_str(SyntaxKind::EnumKeyword, "enum");

        parser.expect_whitespace();

        parser.expect_identifier_kind(SyntaxKind::TypeName, "expected enum name");

        parser.skip_whitespace();

        parser.expect_char('{');

        parser.skip_whitespace();

        CSTEnumVariants::try_parse(parser);

        parser.skip_whitespace();

        parser.expect_char('}');

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTEnumDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let generics = self.generic_names().and_then(|names| names.lower(ctx));

        let variants = self
            .enum_variants()
            .and_then(|variants| variants.lower(ctx));

        Some(ParsedItemKind::EnumDeclaration {
            name_span,
            name: name.to_owned(),
            generics: generics.unwrap_or_default(),
            variants: variants.unwrap_or_default(),
        })
    }
}
