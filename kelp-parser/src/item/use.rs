use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTUseItem, CSTUseTree},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTUseItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        marker.start_node(parser, SyntaxKind::UseItem);
        parser.bump_identifier_kind(SyntaxKind::UseKeyword, "use");
        let parsed_whitespace = parser.expect_whitespace();

        if !CSTUseTree::try_parse(parser) && parsed_whitespace {
            parser.error("Expected use tree");
        }

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTUseItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let tree = self.use_tree()?.lower(ctx)?;

        Some(ParsedItemKind::Use(tree))
    }
}
