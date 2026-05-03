use kelp_core::high::{
    coordinate::Coordinates, semantic_analysis::SemanticAnalysisContext,
    supports_expression_sigil::SupportsExpressionSigil,
};

use crate::{
    coordinates::{
        local::{lower_local_coordinate, parse_local_coordinate},
        world::{lower_world_coordinate, try_parse_world_coordinate},
    },
    cst::{CSTActualCoordinates, CSTCoordinates},
    expression_sigil::{lower_expression_sigil, try_parse_expression_sigil},
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod local;
pub mod world;

#[must_use]
pub fn try_parse_coordinates(parser: &mut Parser) -> bool {
    if try_parse_expression_sigil(parser) {
        return true;
    }

    let Some(char) = parser.peek_char() else {
        return false;
    };

    let state = parser.save_state();

    if char == '^' {
        parser.start_node(SyntaxKind::LocalCoordinates);
        parse_local_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_local_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_local_coordinate(parser);
    } else {
        parser.start_node(SyntaxKind::WorldCoordinates);

        if !try_parse_world_coordinate(parser) {
            parser.restore_state(state);
            return false;
        }

        parser.expect_inline_whitespace();
        try_parse_world_coordinate(parser);
        parser.expect_inline_whitespace();
        try_parse_world_coordinate(parser);
    }

    parser.finish_node();

    true
}

#[must_use]
pub fn lower_actual_coordinates(
    node: CSTActualCoordinates,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Coordinates> {
    Some(match node {
        CSTActualCoordinates::WorldCoordinates(node) => {
            let mut coordinates = node.world_coordinates();

            let x = lower_world_coordinate(coordinates.next()?, ctx)?;
            let y = lower_world_coordinate(coordinates.next()?, ctx)?;
            let z = lower_world_coordinate(coordinates.next()?, ctx)?;

            Coordinates::World(x, y, z)
        }
        CSTActualCoordinates::LocalCoordinates(node) => {
            let mut coordinates = node
                .local_coordinates()
                .map(|coordinate| lower_local_coordinate(coordinate, ctx));

            let x = coordinates.next().unwrap()?;
            let y = coordinates.next().unwrap()?;
            let z = coordinates.next().unwrap()?;

            Coordinates::Local(x, y, z)
        }
    })
}

#[must_use]
pub fn lower_coordinates(
    node: CSTCoordinates,
    ctx: &mut SemanticAnalysisContext,
) -> Option<SupportsExpressionSigil<Coordinates>> {
    match node {
        CSTCoordinates::ActualCoordinates(node) => {
            lower_actual_coordinates(node, ctx).map(SupportsExpressionSigil::Regular)
        }
        CSTCoordinates::ExpressionSigil(node) => lower_expression_sigil(node, ctx),
    }
}
