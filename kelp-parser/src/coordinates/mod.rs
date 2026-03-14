use minecraft_command_types::coordinate::Coordinates;

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
pub fn lower_coordinates(node: CSTCoordinates) -> Option<Coordinates> {
    Some(match node {
        CSTCoordinates::WorldCoordinates(node) => {
            let mut coordinates = node.world_coordinates().filter_map(lower_world_coordinate);

            let x = coordinates.next()?;
            let y = coordinates.next()?;
            let z = coordinates.next()?;

            Coordinates::World(x, y, z)
        }
        CSTCoordinates::LocalCoordinates(node) => {
            let mut coordinates = node.local_coordinates().filter_map(lower_local_coordinate);

            let x = coordinates.next()?;
            let y = coordinates.next()?;
            let z = coordinates.next()?;

            Coordinates::Local(x, y, z)
        }
    })
}
