use kelp_core::high::{
    pattern::{Pattern, PatternKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTScorePattern,
    parser::Parser,
    player_score::{lower_player_score, try_parse_player_score},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_score_pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_player_score(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::ScorePattern);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_score_pattern(
    node: CSTScorePattern,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let player_score = lower_player_score(node.player_score()?, ctx)?;

    Some(PatternKind::Score(player_score).with_span(span))
}
