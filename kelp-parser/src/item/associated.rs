use kelp_core::parsed::item::ParsedItem;
use kelp_core::visibility::Visibility;

use crate::cst::{CSTFunctionDeclarationItem, CSTTypeAliasDeclarationItem};
use crate::extension_traits::{AstNodeExt as _, LowerableAstNode, ParsableAstNode};
use crate::lower_context::LowerContext;
use crate::{
    cst::{CSTAssociatedItem, CSTAssociatedItemKind},
    item::recover_item,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTAssociatedItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::AssociatedItem);

        let parsed_visibility = if parser.try_parse_identifier_kind("pub", SyntaxKind::PubKeyword) {
            parser.expect_whitespace();

            true
        } else {
            false
        };

        let Some(identifier) = parser.peek_identifier() else {
            if !parsed_visibility {
                state.restore(parser);

                return false;
            }

            recover_item(parser, "Expected associated item");

            parser.finish_node();

            return false;
        };

        match identifier {
            "recursive" | "runtime" | "fn" => {
                CSTFunctionDeclarationItem::expect(parser, "Expected function declaration item");
            }
            "type" => {
                CSTTypeAliasDeclarationItem::expect(parser, "Expected type alias declaration");
            }
            _ => {
                parser.error_with_len("Expected associated item", identifier.len());
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTAssociatedItem {
    type Lowered = ParsedItem;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let visibility = if self.pub_token().is_some() {
            Visibility::Public
        } else {
            Visibility::None
        };

        let kind = match self.associated_item_kind()? {
            CSTAssociatedItemKind::FunctionDeclarationItem(node) => node.lower(ctx),
            CSTAssociatedItemKind::TypeAliasDeclarationItem(node) => node.lower(ctx),
        }?;

        Some(ParsedItem {
            span: self.span(),
            visibility,
            kind,
        })
    }
}
