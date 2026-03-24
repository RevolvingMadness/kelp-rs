use kelp_core::high::{item::Item, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTRoot,
    item::{expect_item, lower_item},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_root(parser: &mut Parser) {
    parser.start_node(SyntaxKind::Root);

    loop {
        parser.skip_whitespace();

        if parser.is_eof() {
            break;
        }

        expect_item(parser);
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_root(root: &CSTRoot, ctx: &mut SemanticAnalysisContext) -> Vec<Item> {
    root.items()
        .filter_map(|item| lower_item(item, ctx))
        .collect()
}
