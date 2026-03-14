use kelp_core::item::{Item, ItemKind};

use crate::{
    cst::CSTTypeAliasDeclarationItem,
    data_type::{
        generics::{lower_generic_names, try_parse_generic_names},
        lower_data_type, try_parse_data_type,
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_type_alias_declaration_item(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::TypeAliasDeclarationItem);
    parser.bump_str(SyntaxKind::TypeKeyword, "type");
    parser.skip_inline_whitespace();

    if !parser.expect_identifier_kind(SyntaxKind::DataTypeName, "Expected type name") {
        parser.restore_state(state);

        return false;
    }

    parser.skip_inline_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_inline_whitespace();
    }

    parser.expect_char('=', "Expected '='");

    parser.skip_inline_whitespace();

    if !try_parse_data_type(parser) {
        parser.error("Expected data type");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_type_alias_declaration_item(node: CSTTypeAliasDeclarationItem) -> Option<Item> {
    let span = span_of_cst_node(&node);

    let name_token = node.name()?;
    let name = name_token.text();
    let generic_names = node.generic_names().and_then(lower_generic_names);
    let data_type = lower_data_type(node.data_type()?)?;

    Some(
        ItemKind::TypeAliasDeclaration(
            name.to_owned(),
            generic_names.unwrap_or_default(),
            data_type,
        )
        .with_span(span),
    )
}
