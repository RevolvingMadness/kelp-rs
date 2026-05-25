use kelp_core::high::item::Item;
use kelp_core::visibility::Visibility;
use la_arena::Idx;

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

    if parser.peek_identifier() == Some("pub") {
        parser.bump_str(SyntaxKind::PubKeyword, "pub");
        parser.expect_whitespace();
    }

    let Some(identifier) = parser.peek_identifier() else {
        parser.error("Expected associated item");

        recover_item(parser);

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
pub fn lower_associated_item(node: CSTAssociatedItem, ctx: &mut LowerContext) -> Option<Idx<Item>> {
    let span = span_of_cst_node(&node);

    let visibility = if node.pub_keyword_token().is_some() {
        Visibility::Public
    } else {
        Visibility::None
    };

    let item = match node.associated_item_kind()? {
        CSTAssociatedItemKind::FunctionDeclarationItem(fn_node) => {
            Item::FunctionDeclaration(lower_function_declaration_item_kind(fn_node, ctx)?)
        }
        CSTAssociatedItemKind::TypeAliasDeclarationItem(type_node) => {
            Item::TypeAliasDeclaration(lower_type_alias_declaration_item(type_node)?)
        }
    };

    Some(ctx.allocator.allocate_item(span, visibility, item))
}
