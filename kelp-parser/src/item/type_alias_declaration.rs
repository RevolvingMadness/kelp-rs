use kelp_core::high::item::ItemKind;

use crate::{
    cst::CSTTypeAliasDeclarationItem,
    data_type::{
        generics::{lower_generic_names, try_parse_generic_names},
        lower_data_type, try_parse_data_type,
    },
    parser::Parser,
    span::text_range_to_span,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_type_alias_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::TypeAliasDeclarationItem);
    parser.bump_str(SyntaxKind::TypeKeyword, "type");
    parser.skip_inline_whitespace();

    if !parser.try_bump_identifier_kind(SyntaxKind::DataTypeName) {
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

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
pub fn expect_type_alias_declaration_item_kind(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::TypeAliasDeclarationItem);
    parser.bump_str(SyntaxKind::TypeKeyword, "type");
    parser.skip_inline_whitespace();

    parser.expect_identifier_kind(SyntaxKind::DataTypeName, "Expected type name");

    parser.skip_inline_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_inline_whitespace();
    }

    parser.expect_char('=', "Expected '='");

    parser.skip_inline_whitespace();

    if !try_parse_data_type(parser) {
        parser.error("Expected data type");
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_type_alias_declaration_item(node: CSTTypeAliasDeclarationItem) -> Option<ItemKind> {
    let name_token = node.name()?;
    let name_span = text_range_to_span(name_token.text_range());
    let name = name_token.text();
    let generic_names = node.generic_names().and_then(lower_generic_names);
    let data_type = lower_data_type(node.data_type()?)?;

    Some(ItemKind::TypeAliasDeclaration(
        name_span,
        name.to_owned(),
        generic_names.unwrap_or_default(),
        data_type,
    ))
}
