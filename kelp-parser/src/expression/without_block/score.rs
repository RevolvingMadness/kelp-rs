use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTScoreExpression,
    lower_context::LowerContext,
    parser::Parser,
    player_score::{lower_player_score, try_parse_player_score},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_score_expression(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_player_score(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::ScoreExpression);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_score_expression(
    node: CSTScoreExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = span_of_cst_node(&node);

    let player_score = lower_player_score(node.player_score()?, ctx)?;

    Some(ParsedExpressionKind::PlayerScore(player_score).with_span(span))
}
