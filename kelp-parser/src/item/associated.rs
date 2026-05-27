use kelp_core::parsed::item::ParsedItem;
use kelp_core::visibility::Visibility;

use crate::lower_context::LowerContext;
use crate::{
    cst::{CSTAssociatedItem, CSTAssociatedItemKind},
    item::{
        function_declaration::{
            expect_function_declaration_item_kind, lower_function_declaration_item_kind,
        },
        recover_item,
        type_alias_declaration::{
            expect_type_alias_declaration_item_kind, lower_type_alias_declaration_item,
        },
    },
    parser::Parser,
    span::span_of_cst_node,
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
        "type" => expect_type_alias_declaration_item_kind(parser),
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
    let span = span_of_cst_node(&node);

    let visibility = if node.pub_keyword_token().is_some() {
        Visibility::Public
    } else {
        Visibility::None
    };

    let kind = match node.associated_item_kind()? {
        CSTAssociatedItemKind::FunctionDeclarationItem(fn_node) => {
            lower_function_declaration_item_kind(fn_node, ctx)?
        }
        CSTAssociatedItemKind::TypeAliasDeclarationItem(type_node) => {
            lower_type_alias_declaration_item(type_node)?
        }
    };

    Some(ParsedItem {
        span,
        visibility,
        kind,
    })
}
