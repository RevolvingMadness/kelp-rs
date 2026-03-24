use kelp_core::high::item::ItemKind;

use crate::{
    cst::CSTUseItem,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
    use_tree::{lower_use_tree, try_parse_use_tree},
};

#[must_use]
pub fn try_parse_use_item_kind(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    parser.start_node_at(checkpoint, SyntaxKind::UseItem);
    parser.bump_identifier_kind(SyntaxKind::UseKeyword, "use");
    let parsed_whitespace = parser.expect_whitespace();

    if !try_parse_use_tree(parser) && parsed_whitespace {
        parser.error("Expected use tree");
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
pub fn expect_use_item_kind(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    parser.start_node_at(checkpoint, SyntaxKind::UseItem);
    parser.bump_identifier_kind(SyntaxKind::UseKeyword, "use");

    parser.expect_whitespace();

    if !try_parse_use_tree(parser) {
        parser.error("Expected use tree");
    }

    expect_semicolon_ending(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_use_item(node: CSTUseItem) -> Option<ItemKind> {
    let tree = lower_use_tree(node.use_tree()?)?;

    Some(ItemKind::Use(tree))
}
