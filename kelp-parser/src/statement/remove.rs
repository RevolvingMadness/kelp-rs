use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::{CSTExpression, CSTRemoveStatement},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTRemoveStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::RemoveStatement);

        parser.bump_str(SyntaxKind::RemoveKeyword, "remove");

        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTRemoveStatement {
    type Lowered = ParsedStatement;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = self.target()?.lower(ctx)?;

        Some(ParsedStatementKind::Remove(target).with_span(self.span()))
    }
}
