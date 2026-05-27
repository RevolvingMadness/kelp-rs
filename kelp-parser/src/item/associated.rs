use kelp_core::parsed::item::ParsedItem;
use kelp_core::visibility::Visibility;

use crate::cst::CSTTypeAliasDeclarationItem;
use crate::extension_traits::{AstNodeExt as _, LowerableAstNode, ParsableAstNode};
use crate::lower_context::LowerContext;
use crate::{
    cst::{CSTAssociatedItem, CSTAssociatedItemKind},
    item::{
        function_declaration::{
            expect_function_declaration_item_kind, lower_function_declaration_item_kind,
        },
        recover_item,
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn expect_associated_item(parser: &mut Parser) {
    parser.start_node(SyntaxKind::AssociatedItem);

    if parser.try_parse_identifier_kind("pub", SyntaxKind::PubKeyword) {
        parser.expect_whitespace();
    }

    let Some(identifier) = parser.peek_identifier() else {
        recover_item(parser, "Expected associated item");

        parser.finish_node();

        return;
    };

    match identifier {
        "recursive" | "runtime" | "fn" => expect_function_declaration_item_kind(parser),
        "type" => {
            CSTTypeAliasDeclarationItem::expect(parser, "Expected type alias declaration");
        }
        _ => {
            parser.error_with_len("Expected associated item", identifier.len());

            parser.add_token(SyntaxKind::Garbage, identifier.len());
        }
    }

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_associated_item(
    node: CSTAssociatedItem,
    ctx: &mut LowerContext,
) -> Option<ParsedItem> {
    let span = node.span();

    let visibility = if node.pub_token().is_some() {
        Visibility::Public
    } else {
        Visibility::None
    };

    let kind = match node.associated_item_kind()? {
        CSTAssociatedItemKind::FunctionDeclarationItem(node) => {
            lower_function_declaration_item_kind(node, ctx)?
        }
        CSTAssociatedItemKind::TypeAliasDeclarationItem(node) => node.lower(ctx)?,
    };

    Some(ParsedItem {
        span,
        visibility,
        kind,
    })
}
