use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTBlockExpression, CSTExpression, CSTIteratorLoopExpression, CSTPattern},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTIteratorLoopExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::IteratorLoopExpression);
        parser.bump_str(SyntaxKind::ForKeyword, "for");
        parser.skip_inline_whitespace();

        if !CSTPattern::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.skip_whitespace();

        if !parser.expect_str("in", SyntaxKind::InKeyword, "Expected 'in'") {
            parser.bump_until_whitespace();
        }

        parser.skip_whitespace();

        CSTExpression::expect(parser, "Expected loop iterable");

        parser.skip_whitespace();

        CSTBlockExpression::expect(parser, "Expected loop body");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTIteratorLoopExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let pattern = self.pattern()?.lower(ctx)?;

        let expression = self.expression()?.lower(ctx)?;

        let body = self.block_expression()?.lower(ctx)?;

        Some(
            ParsedExpressionKind::ForLoop(false, pattern, Box::new(expression), Box::new(body))
                .with_span(self.span()),
        )
    }
}
