use kelp_core::item::Item;

use crate::{
    cst::CSTRoot,
    lower::{item::lower_item, statement::item::try_parse_item},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_root(parser: &mut Parser) {
    parser.start_node(SyntaxKind::Root);

    parser.skip_whitespace();

    let mut is_first = true;

    loop {
        if parser.is_eof() {
            break;
        }

        if !is_first && !parser.try_parse_newline_whitespace() {
            parser.recover_newline("Expected newline to mark end of item");

            is_first = false;

            continue;
        }

        let _ = try_parse_item(parser);

        is_first = false;
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_root(root: &CSTRoot) -> Vec<Item> {
    root.items().filter_map(lower_item).collect()
}
