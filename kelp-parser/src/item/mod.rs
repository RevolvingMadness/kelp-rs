use kelp_core::high::{item::Item, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTItem,
    data_type::generics::lower_generic_names,
    item::{
        mcfn_declaration::{lower_mcfn_declaration_item, try_parse_mcfn_declaration_item},
        module_declaration::{lower_module_declaration_item, try_parse_module_declaration_item},
        struct_declaration::{
            lower_struct_declaration_item_field, try_parse_struct_declaration_item,
        },
        type_alias_declaration::{
            lower_type_alias_declaration_item, try_parse_type_alias_declaration_item,
        },
        r#use::{lower_use_item, try_parse_use_item},
    },
    parser::Parser,
    span::text_range_to_span,
};

pub mod mcfn_declaration;
pub mod module_declaration;
pub mod struct_declaration;
pub mod type_alias_declaration;
pub mod r#use;

#[must_use]
pub fn try_parse_item(parser: &mut Parser) -> bool {
    let Some(identifier) = parser.peek_identifier() else {
        return false;
    };

    match identifier {
        "use" => try_parse_use_item(parser),
        "mod" => try_parse_module_declaration_item(parser),
        "mcfn" => try_parse_mcfn_declaration_item(parser),
        "struct" => try_parse_struct_declaration_item(parser),
        "type" => try_parse_type_alias_declaration_item(parser),
        _ => false,
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item(node: CSTItem, ctx: &mut SemanticAnalysisContext) -> Option<Item> {
    match node {
        CSTItem::ModuleDeclarationItem(node) => lower_module_declaration_item(node, ctx),
        CSTItem::MCFNDeclarationItem(node) => lower_mcfn_declaration_item(node, ctx),
        CSTItem::StructDeclarationItem(node) => {
            let struct_name_token = node.name()?;
            let struct_name_span = text_range_to_span(struct_name_token.text_range());
            let struct_name = struct_name_token.text().to_owned();

            let generics = node.generic_names().and_then(lower_generic_names);

            let fields = node
                .fields()
                .filter_map(lower_struct_declaration_item_field)
                .collect();

            Some(Item::StructDeclaration(
                struct_name_span,
                struct_name,
                generics.unwrap_or_default(),
                fields,
            ))
        }
        CSTItem::TypeAliasDeclarationItem(node) => lower_type_alias_declaration_item(node),
        CSTItem::UseItem(node) => lower_use_item(node),
    }
}
