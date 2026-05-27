use kelp_core::{parsed::item::ParsedItemKind, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{CSTItem, CSTModuleDeclarationItem},
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTModuleDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::ModuleDeclarationItem);
        parser.bump_str(SyntaxKind::ModKeyword, "mod");

        parser.expect_whitespace();

        if !parser.try_bump_identifier_kind(SyntaxKind::ModuleName) {
            state.restore(parser);

            return false;
        }

        parser.skip_whitespace();

        parser.expect_char('{', "Expected '{'");

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            CSTItem::expect(parser, "Expected item");
        }

        parser.expect_char('}', "Expected '}'");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTModuleDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.module_name_token()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let items = self
            .items()
            .map(|item| item.lower(ctx))
            .collect_option_all()?;

        Some(ParsedItemKind::ModuleDeclaration {
            name_span,
            name: name.to_owned(),
            items,
        })
    }
}
