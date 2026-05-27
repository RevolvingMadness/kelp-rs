use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTBlockExpression, CSTExpression, CSTPredicateLoopExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTPredicateLoopExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::PredicateLoopExpression);
        parser.bump_str(SyntaxKind::WhileKeyword, "while");
        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.skip_inline_whitespace();

        CSTBlockExpression::expect(parser, "Expected loop body");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTPredicateLoopExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let condition = self.expression()?.lower(ctx)?;
        let body = self.block_expression()?.lower(ctx)?;

        Some(
            ParsedExpressionKind::WhileLoop(Box::new(condition), Box::new(body))
                .with_span(self.span()),
        )
    }
}
