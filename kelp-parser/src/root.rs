use kelp_core::high::{item::Item, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTRoot,
    item::{expect_item, lower_item},
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

        expect_item(parser);

        is_first = false;
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_root(root: &CSTRoot, ctx: &mut SemanticAnalysisContext) -> Vec<Item> {
    root.items()
        .filter_map(|item| lower_item(item, ctx))
        .collect()
}
