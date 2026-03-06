use kelp_core::{
    expression::{Expression, ExpressionKind},
    high::player_score::HighPlayerScore,
};

use crate::{
    cst::CSTScoreExpression,
    lower::entity_selector::{lower_entity_selector, try_parse_entity_selector},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_score_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ScoreExpression);
    parser.bump_identifier_kind(SyntaxKind::ScoreKeyword, "score");

    if !parser.expect_inline_whitespace() || !try_parse_entity_selector(parser) {
        parser.restore_state(state);

        return false;
    }

    parser.expect_inline_whitespace();

    parser.expect_identifier("Expected scoreboard objective");

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_score_expression(node: CSTScoreExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let selector = lower_entity_selector(node.entity_selector()?)?;
    let objective = node.identifier_token()?.to_string();

    Some(
        ExpressionKind::PlayerScore(HighPlayerScore {
            is_generated: false,
            selector,
            objective,
        })
        .with_span(span),
    )
}
