use kelp_core::{
    high::item::{Item, ItemKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTItem,
    data_type::generics::lower_generic_names,
    item::{
        mcfn_declaration::lower_mcfn_declaration_item,
        struct_declaration::lower_struct_declaration_item_field,
        type_alias_declaration::lower_type_alias_declaration_item,
    },
    span::{span_of_cst_node, text_range_to_span},
};

pub mod mcfn_declaration;
pub mod struct_declaration;
pub mod type_alias_declaration;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_item(node: CSTItem, ctx: &mut SemanticAnalysisContext) -> Option<Item> {
    match node {
        CSTItem::MCFNDeclarationItem(node) => lower_mcfn_declaration_item(node, ctx),
        CSTItem::StructDeclarationItem(node) => {
            let span = span_of_cst_node(&node);

            let struct_name_token = node.name()?;
            let struct_name_span = text_range_to_span(struct_name_token.text_range());
            let struct_name = struct_name_token.text().to_owned();

            let generics = node.generic_names().and_then(lower_generic_names);

            let fields = node
                .fields()
                .filter_map(lower_struct_declaration_item_field)
                .collect();

            Some(
                ItemKind::StructDeclaration(
                    struct_name_span,
                    struct_name,
                    generics.unwrap_or_default(),
                    fields,
                )
                .with_span(span),
            )
        }
        CSTItem::TypeAliasDeclarationItem(node) => lower_type_alias_declaration_item(node),
    }
}
