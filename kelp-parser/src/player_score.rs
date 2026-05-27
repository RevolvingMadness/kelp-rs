use kelp_core::parsed::player_score::PlayerScore;

use crate::{
    cst::CSTPlayerScore,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_player_score(parser: &mut Parser) -> bool {
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

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_player_score(node: CSTPlayerScore, ctx: &mut LowerContext) -> Option<PlayerScore> {
    let selector = lower_entity_selector(node.entity_selector()?, ctx)?;

    let objective_token = node.scoreboard_objective_token()?;
    let objective = objective_token.text();

    Some(PlayerScore {
        is_generated: false,
        selector: Box::new(selector),
        objective: objective.to_owned(),
    })
}
