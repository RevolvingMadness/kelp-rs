use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTScorePattern,
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    player_score::{lower_player_score, try_parse_player_score},
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTScorePattern {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !try_parse_player_score(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::ScorePattern);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTScorePattern {
    type Lowered = ParsedPattern;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let player_score = lower_player_score(self.player_score()?, ctx)?;

        Some(ParsedPatternKind::Score(player_score).with_span(self.span()))
    }
}
