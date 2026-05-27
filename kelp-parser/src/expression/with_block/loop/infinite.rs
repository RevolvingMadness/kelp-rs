use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTBlockExpression, CSTInfiniteLoopExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTInfiniteLoopExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::InfiniteLoopExpression);
        parser.bump_str(SyntaxKind::LoopKeyword, "loop");
        parser.skip_inline_whitespace();

        if !CSTBlockExpression::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTInfiniteLoopExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let body = self.block_expression()?.lower(ctx)?;

        Some(ParsedExpressionKind::Loop(Box::new(body)).with_span(self.span()))
    }
}
