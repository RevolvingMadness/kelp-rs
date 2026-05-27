use kelp_core::parsed::player_score::ParsedPlayerScore;

use crate::{
    cst::CSTPlayerScore,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTPlayerScore {
    fn try_parse(parser: &mut Parser) -> bool {
        let checkpoint = parser.mark();
        let state = parser.save_state();

        if !parser.try_bump_str("score", SyntaxKind::ScoreKeyword) {
            return false;
        }

        if !parser.expect_inline_whitespace() {
            state.restore(parser);

            return false;
        }

        if !try_parse_entity_selector(parser) {
            state.restore(parser);

            return false;
        }

        checkpoint.start_node(parser, SyntaxKind::PlayerScore);

        parser.expect_inline_whitespace();

        parser.expect_identifier_kind(
            SyntaxKind::ScoreboardObjective,
            "Expected scoreboard objective",
        );

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTPlayerScore {
    type Lowered = ParsedPlayerScore;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let selector = lower_entity_selector(self.entity_selector()?, ctx)?;

        let objective_token = self.scoreboard_objective_token()?;
        let objective = objective_token.text();

        Some(ParsedPlayerScore {
            is_generated: false,
            selector: Box::new(selector),
            objective: objective.to_owned(),
        })
    }
}
