use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTItemStatement,
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    item::{lower_item, try_parse_item},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTItemStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !try_parse_item(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::ItemStatement);
        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTItemStatement {
    type Lowered = ParsedStatement;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let item = lower_item(self.item()?, ctx)?;

        Some(ParsedStatementKind::Item(Box::new(item)).with_span(self.span()))
    }
}
