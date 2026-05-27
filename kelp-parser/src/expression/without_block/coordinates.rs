use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTCoordinates, CSTCoordinatesExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_coordinates_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::CoordinatesExpression);

    parser.bump_identifier_kind(SyntaxKind::CoordinatesKeyword, "coordinates");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        state.restore(parser);

        return false;
    }

    parser.skip_whitespace();

    if !CSTCoordinates::try_parse(parser) {
        parser.error("Expected coordinates");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_coordinates_expression(
    node: CSTCoordinatesExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let coordinates = node.coordinates()?.lower(ctx)?;

    let span = node.span();

    Some(ParsedExpressionKind::Coordinates(Box::new(coordinates)).with_span(span))
}
