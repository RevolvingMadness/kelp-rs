use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::{CSTAppendStatement, CSTExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    statement::expect_semicolon_ending,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTAppendStatement {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::AppendStatement);

        parser.bump_str(SyntaxKind::AppendKeyword, "append");

        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            parser.error("Expected expression");
        }

        expect_semicolon_ending(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTAppendStatement {
    type Lowered = ParsedStatement;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = self.target()?.lower(ctx)?;
        let value = self.value()?.lower(ctx)?;

        Some(ParsedStatementKind::Append(target, Box::new(value)).with_span(self.span()))
    }
}
