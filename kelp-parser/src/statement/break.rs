use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTBreakStatement,
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTBreakStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::BreakStatement);

        parser.bump_str(SyntaxKind::BreakKeyword, "break");

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTBreakStatement {
    type Lowered = ParsedStatement;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedStatementKind::Break.with_span(self.span()))
    }
}
