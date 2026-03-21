use kelp_core::high::{
    player_score::PlayerScore, semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTPlayerScore,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_player_score(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    let state = parser.save_state();

    if !parser.try_bump_str("score", SyntaxKind::ScoreKeyword) {
        return false;
    }

    if !parser.expect_inline_whitespace() {
        parser.restore_state(state);

        return false;
    }

    if !try_parse_entity_selector(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::PlayerScore);

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
pub fn lower_player_score(
    node: CSTPlayerScore,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<PlayerScore> {
    let selector = lower_entity_selector(node.entity_selector()?)?;

    let objective_token = node.scoreboard_objective_token()?;
    let objective = objective_token.text();

    Some(PlayerScore {
        is_generated: false,
        selector,
        objective: objective.to_owned(),
    })
}
