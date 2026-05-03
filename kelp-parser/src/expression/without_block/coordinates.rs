use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    coordinates::{lower_coordinates, try_parse_coordinates},
    cst::CSTCoordinatesExpression,
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_coordinates_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::CoordinatesExpression);

    parser.bump_identifier_kind(SyntaxKind::CoordinatesKeyword, "coordinates");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !try_parse_coordinates(parser) {
        parser.error("Expected coordinates");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_coordinates_expression(
    node: CSTCoordinatesExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let coordinates = lower_coordinates(node.coordinates()?, ctx)?;

    let span = span_of_cst_node(&node);

    Some(ExpressionKind::Coordinates(Box::new(coordinates)).with_span(span))
}
