use kelp_core::{
    parsed::item::{ParsedItem, ParsedItemKind},
    visibility::Visibility,
};

use crate::{
    cst::{CSTItem, CSTItemKind},
    data_type::generics::lower_generic_names,
    item::{
        function_declaration::{
            expect_function_declaration_item_kind, lower_function_declaration_item_kind,
            try_parse_function_declaration_item_kind,
        },
        implementation::inherent::{
            expect_inherent_implementation_item_kind, lower_inherent_implementation_item,
            try_parse_inherent_implementation_item_kind,
        },
        minecraft_function_declaration::{
            expect_minecraft_function_declaration_item_kind,
            lower_minecraft_function_declaration_item_kind,
            try_parse_minecraft_function_declaration_item_kind,
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
    lower_context::LowerContext,
    parser::Parser,
    span::{span_of_cst_node, text_range_to_span},
    r#struct::{lower_struct_fields, lower_tuple_fields},
    syntax::SyntaxKind,
};

pub mod associated;
pub mod function_declaration;
pub mod implementation;
pub mod minecraft_function_declaration;
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
        "impl" => try_parse_inherent_implementation_item_kind(parser),
        "use" => try_parse_use_item_kind(parser),
        "mod" => try_parse_module_declaration_item_kind(parser),
        "recursive" | "runtime" | "fn" => try_parse_function_declaration_item_kind(parser),
        "mcfn" => try_parse_minecraft_function_declaration_item_kind(parser),
        "struct" => try_parse_struct_declaration_item_kind(parser),
        "type" => try_parse_type_alias_declaration_item_kind(parser),
        _ => false,
    }
}

fn recover_item(parser: &mut Parser) {
    let bytes = &parser.source.as_bytes()[parser.pos..];

    let (not_end, length) = bytes
        .iter()
        .position(|&b| b == b'\n' || b == b';')
        .map_or((false, bytes.len()), |value| (true, value));

    parser.add_token(SyntaxKind::Garbage, length + usize::from(not_end));
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
    parser.start_node(SyntaxKind::Item);

    if parser.peek_identifier() == Some("pub") {
        parser.bump_str(SyntaxKind::PubKeyword, "pub");

        Some(parser.expect_whitespace())
    } else {
        None
    };

    let Some(identifier) = parser.peek_identifier() else {
        parser.error("Expected item");

        recover_item(parser);

        return true;
    };

    match identifier {
        "impl" => expect_inherent_implementation_item_kind(parser),
        "use" => expect_use_item_kind(parser),
        "mod" => expect_module_declaration_item_kind(parser),
        "recursive" | "runtime" | "fn" => expect_function_declaration_item_kind(parser),
        "mcfn" => expect_minecraft_function_declaration_item_kind(parser),
        "struct" => expect_struct_declaration_item_kind(parser),
        "type" => expect_type_alias_declaration_item_kind(parser),
        _ => {
            parser.error_with_len("Expected item", identifier.len());

            parser.add_token(SyntaxKind::Garbage, identifier.len());
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_item_kind(node: CSTItemKind, ctx: &mut LowerContext) -> Option<ParsedItemKind> {
    match node {
        CSTItemKind::InherentImplementationItem(node) => {
            lower_inherent_implementation_item(node, ctx)
        }
        CSTItemKind::ModuleDeclarationItem(node) => lower_module_declaration_item(node, ctx),
        CSTItemKind::FunctionDeclarationItem(node) => {
            lower_function_declaration_item_kind(node, ctx)
        }
        CSTItemKind::MinecraftFunctionDeclarationItem(node) => {
            lower_minecraft_function_declaration_item_kind(node, ctx)
        }
        CSTItemKind::RegularStructDeclarationItem(node) => {
            let name_token = node.name()?;
            let name_range = name_token.text_range();
            let name = name_token.text();

            let generic_names = node.generic_names().and_then(lower_generic_names);

            let field_types = node.struct_fields().and_then(lower_struct_fields);

            Some(ParsedItemKind::RegularStructDeclaration {
                name_span: text_range_to_span(name_range),
                name: name.to_owned(),
                generic_names: generic_names.unwrap_or_default(),
                field_types: field_types.unwrap_or_default(),

                id: None,
                generic_ids: None,
            })
        }
        CSTItemKind::TupleStructDeclarationItem(node) => {
            let name_token = node.name()?;
            let name_range = name_token.text_range();
            let name = name_token.text();

            let generic_names = node.generic_names().and_then(lower_generic_names);

            let field_types = node.tuple_fields().and_then(lower_tuple_fields);

            Some(ParsedItemKind::TupleStructDeclaration {
                name_span: text_range_to_span(name_range),
                name: name.to_owned(),
                generic_names: generic_names.unwrap_or_default(),
                field_types: field_types.unwrap_or_default(),

                id: None,
                generic_ids: None,
            })
        }
        CSTItemKind::TypeAliasDeclarationItem(node) => lower_type_alias_declaration_item(node),
        CSTItemKind::UseItem(node) => lower_use_item(node),
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item(node: CSTItem, ctx: &mut LowerContext) -> Option<ParsedItem> {
    let span = span_of_cst_node(&node);

    let visibility = if node.pub_keyword_token().is_some() {
        Visibility::Public
    } else {
        Visibility::None
    };

    let kind = lower_item_kind(node.item_kind()?, ctx)?;

    Some(ParsedItem {
        span,
        visibility,
        kind,
    })
}
