use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTContinueStatement,
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTContinueStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::ContinueStatement);

        parser.bump_str(SyntaxKind::ContinueKeyword, "continue");

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTContinueStatement {
    type Lowered = ParsedStatement;

    fn lower(self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedStatementKind::Continue.with_span(self.span()))
    }
}
