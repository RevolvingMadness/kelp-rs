use kelp_core::high::item::Item;

use crate::{
    cst::CSTUseItem,
    parser::Parser,
    syntax::SyntaxKind,
    use_tree::{lower_use_tree, try_parse_use_tree},
};

#[must_use]
pub fn try_parse_use_item(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if parser.peek_identifier() != Some("use") {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::UseItem);
    parser.bump_identifier_kind(SyntaxKind::UseKeyword, "use");
    parser.skip_inline_whitespace();

    if !try_parse_use_tree(parser) {
        parser.error("Expected use tree");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_use_item(node: CSTUseItem) -> Option<Item> {
    let tree = lower_use_tree(node.use_tree()?)?;

    Some(Item::Use(tree))
}
