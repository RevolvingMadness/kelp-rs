use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::CSTInherentImplementationItem,
    data_type::{
        generics::{lower_generic_names, try_parse_generic_names},
        lower_data_type, try_parse_data_type,
    },
    item::associated::{expect_associated_item, lower_associated_item},
    lower_context::LowerContext,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_inherent_implementation_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::InherentImplementationItem);

    if !parser.try_bump_str("impl", SyntaxKind::ImplKeyword) {
        state.restore(parser);

        return false;
    }

    parser.skip_whitespace();

    try_parse_generic_names(parser);

    parser.skip_whitespace();

    if !try_parse_data_type(parser) {
        state.restore(parser);
        return false;
    }

    parser.skip_whitespace();

    if !parser.expect_char('{', "Expected '{'") {
        state.restore(parser);
        return false;
    }

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        expect_associated_item(parser);
    }

    parser.expect_char('}', "Expected '}'");
    parser.finish_node();

    true
}

pub fn expect_inherent_implementation_item_kind(parser: &mut Parser) {
    parser.start_node(SyntaxKind::InherentImplementationItem);
    parser.bump_str(SyntaxKind::ImplKeyword, "impl");

    parser.skip_whitespace();

    try_parse_generic_names(parser);

    parser.skip_whitespace();

    if !try_parse_data_type(parser) {
        parser.error("Expected data type");
    }

    parser.skip_whitespace();
    parser.expect_char('{', "Expected '{'");

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        expect_associated_item(parser);
    }

    parser.expect_char('}', "Expected '}'");

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_inherent_implementation_item(
    node: CSTInherentImplementationItem,
    ctx: &mut LowerContext,
) -> Option<ParsedItemKind> {
    let generic_names = node.generic_names().and_then(lower_generic_names);
    let target_type = node.data_type()?;
    let target_type_span = span_of_cst_node(&target_type);
    let target_type = lower_data_type(target_type)?;

    let associated_items = node
        .associated_items()
        .filter_map(|item| lower_associated_item(item, ctx))
        .collect();

    Some(ParsedItemKind::InherentImplementationItem {
        generic_names: generic_names.unwrap_or_default(),
        target_type_span,
        target_type,
        associated_items,
    })
}
