use kelp_core::high::{
    coordinate::Coordinates, semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    coordinates::{
        local::{lower_local_coordinate, parse_local_coordinate},
        world::{lower_world_coordinate, parse_world_coordinate},
    },
    cst::CSTCoordinates,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod local;
pub mod world;

#[must_use]
pub fn try_parse_coordinates(parser: &mut Parser) -> bool {
    let Some(char) = parser.peek_char() else {
        return false;
    };

    if char == '~' || char.is_numeric() {
        parser.start_node(SyntaxKind::WorldCoordinates);
        parse_world_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_world_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_world_coordinate(parser);
        parser.finish_node();

        true
    } else if char == '^' {
        parser.start_node(SyntaxKind::LocalCoordinates);
        parse_local_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_local_coordinate(parser);
        parser.expect_inline_whitespace();
        parse_local_coordinate(parser);
        parser.finish_node();

        true
    } else {
        false
    }
}

#[must_use]
pub fn lower_coordinates(
    node: CSTCoordinates,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Coordinates> {
    Some(match node {
        CSTCoordinates::WorldCoordinates(node) => {
            let mut coordinates = node.world_coordinates();

            let x = lower_world_coordinate(coordinates.next()?, ctx)?;
            let y = lower_world_coordinate(coordinates.next()?, ctx)?;
            let z = lower_world_coordinate(coordinates.next()?, ctx)?;

            Coordinates::World(x, y, z)
        }
        CSTCoordinates::LocalCoordinates(node) => {
            let mut coordinates = node
                .local_coordinates()
                .map(|coordinate| lower_local_coordinate(coordinate, ctx));

            let x = coordinates.next()?.unwrap();
            let y = coordinates.next()?.unwrap();
            let z = coordinates.next()?.unwrap();

            Coordinates::Local(x, y, z)
        }
    })
}
