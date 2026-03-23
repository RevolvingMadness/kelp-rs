use kelp_core::{
    high::{
        item::{Item, ItemKind},
        semantic_analysis_context::SemanticAnalysisContext,
    },
    visibility::Visibility,
};

use crate::{
    cst::{CSTItem, CSTItemKind},
    data_type::generics::lower_generic_names,
    item::{
        mcfn_declaration::{
            expect_mcfn_declaration_item_kind, lower_mcfn_declaration_item_kind,
            try_parse_mcfn_declaration_item_kind,
        },
        module_declaration::{
            expect_module_declaration_item_kind, lower_module_declaration_item,
            try_parse_module_declaration_item_kind,
        },
        struct_declaration::{
            expect_struct_declaration_item_kind, try_parse_struct_declaration_item_kind,
        },
        type_alias_declaration::{
            expect_type_alias_declaration_item_kind, lower_type_alias_declaration_item,
            try_parse_type_alias_declaration_item_kind,
        },
        r#use::{expect_use_item_kind, lower_use_item, try_parse_use_item_kind},
    },
    parser::Parser,
    span::text_range_to_span,
    r#struct::{lower_struct_fields, lower_tuple_fields},
    syntax::SyntaxKind,
};

pub mod mcfn_declaration;
pub mod module_declaration;
pub mod struct_declaration;
pub mod type_alias_declaration;
pub mod r#use;

#[must_use]
pub fn try_parse_item_kind(parser: &mut Parser) -> bool {
    let Some(identifier) = parser.peek_identifier() else {
        return false;
    };

    match identifier {
        "use" => try_parse_use_item_kind(parser),
        "mod" => try_parse_module_declaration_item_kind(parser),
        "mcfn" => try_parse_mcfn_declaration_item_kind(parser),
        "struct" => try_parse_struct_declaration_item_kind(parser),
        "type" => try_parse_type_alias_declaration_item_kind(parser),
        _ => false,
    }
}

#[must_use]
pub fn expect_item_kind(parser: &mut Parser) -> bool {
    let Some(identifier) = parser.peek_identifier() else {
        return false;
    };

    match identifier {
        "use" => expect_use_item_kind(parser),
        "mod" => expect_module_declaration_item_kind(parser),
        "mcfn" => expect_mcfn_declaration_item_kind(parser),
        "struct" => expect_struct_declaration_item_kind(parser),
        "type" => expect_type_alias_declaration_item_kind(parser),
        _ => false,
    }
}

#[must_use]
pub fn try_parse_item(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    let parsed_visibility = if parser.peek_identifier() == Some("pub") {
        parser.bump_str(SyntaxKind::PubKeyword, "pub");

        Some(parser.expect_whitespace())
    } else {
        None
    };

    let parsed_item_kind = try_parse_item_kind(parser);

    if !parsed_item_kind {
        match parsed_visibility {
            Some(true) => {
                parser.error("Expected item");
            }
            Some(false) => {}
            None => {
                return false;
            }
        }
    }

    parser.start_node_at(checkpoint, SyntaxKind::Item);

    parser.finish_node();

    true
}

pub fn expect_item(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if parser.peek_identifier() == Some("pub") {
        parser.bump_str(SyntaxKind::PubKeyword, "pub");

        Some(parser.expect_whitespace())
    } else {
        None
    };

    if !expect_item_kind(parser) {
        parser.error("Expected item");
    }

    parser.start_node_at(checkpoint, SyntaxKind::Item);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_item_kind(node: CSTItemKind, ctx: &mut SemanticAnalysisContext) -> Option<ItemKind> {
    match node {
        CSTItemKind::ModuleDeclarationItem(node) => lower_module_declaration_item(node, ctx),
        CSTItemKind::MCFNDeclarationItem(node) => lower_mcfn_declaration_item_kind(node, ctx),
        CSTItemKind::StructStructDeclarationItem(node) => {
            let struct_name_token = node.name()?;
            let struct_name_span = text_range_to_span(struct_name_token.text_range());
            let struct_name = struct_name_token.text().to_owned();

            let generics = node.generic_names().and_then(lower_generic_names);

            let fields = node
                .struct_fields()
                .and_then(lower_struct_fields)
                .unwrap_or_default();

            Some(ItemKind::StructStructDeclaration(
                struct_name_span,
                struct_name,
                generics.unwrap_or_default(),
                fields,
            ))
        }
        CSTItemKind::TupleStructDeclarationItem(node) => {
            let struct_name_token = node.name()?;
            let struct_name_span = text_range_to_span(struct_name_token.text_range());
            let struct_name = struct_name_token.text().to_owned();

            let generics = node.generic_names().and_then(lower_generic_names);

            let fields = node
                .tuple_fields()
                .and_then(lower_tuple_fields)
                .unwrap_or_default();

            Some(ItemKind::TupleStructDeclaration(
                struct_name_span,
                struct_name,
                generics.unwrap_or_default(),
                fields,
            ))
        }
        CSTItemKind::TypeAliasDeclarationItem(node) => lower_type_alias_declaration_item(node),
        CSTItemKind::UseItem(node) => lower_use_item(node),
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item(node: CSTItem, ctx: &mut SemanticAnalysisContext) -> Option<Item> {
    let visibility = if node.pub_keyword_token().is_some() {
        Visibility::Public
    } else {
        Visibility::None
    };

    let kind = lower_item_kind(node.item_kind()?, ctx)?;

    Some(Item { visibility, kind })
}
