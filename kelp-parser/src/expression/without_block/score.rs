use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTPlayerScore, CSTScoreExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTScoreExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let checkpoint = parser.mark();

        if !CSTPlayerScore::try_parse(parser) {
            return false;
        }

        checkpoint.start_node(parser, SyntaxKind::ScoreExpression);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTScoreExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let player_score = self.player_score()?.lower(ctx)?;

        Some(ParsedExpressionKind::PlayerScore(player_score).with_span(self.span()))
    }
}
