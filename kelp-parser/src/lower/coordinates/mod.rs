use minecraft_command_types::coordinate::Coordinates;

use crate::{
    cstlib::CSTNodeType,
    lower::coordinates::{
        local::CSTLocalCoordinates,
        world::{CSTWorldCoordinate, CSTWorldCoordinates},
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod local;
pub mod world;

pub enum CSTCoordinates<'a> {
    World(CSTWorldCoordinates<'a>),
    Local(CSTLocalCoordinates<'a>),
}

impl<'a> CSTCoordinates<'a> {
    pub(crate) fn parse_local_coordinate(parser: &mut Parser) {
        parser.start_node(SyntaxKind::LocalCoordinate);

        if !parser.expect_char('^', "Expected '^'") {
            parser.bump_char();
        }

        parser.try_parse_fractional_value();

        parser.finish_node();
    }

    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let Some(char) = parser.peek_char() else {
            return false;
        };

        if char == '~' || char.is_numeric() {
            parser.start_node(SyntaxKind::WorldCoordinates);
            CSTWorldCoordinate::try_parse(parser);
            parser.expect_inline_whitespace();
            CSTWorldCoordinate::try_parse(parser);
            parser.expect_inline_whitespace();
            CSTWorldCoordinate::try_parse(parser);
            parser.finish_node();

            true
        } else if char == '^' {
            parser.start_node(SyntaxKind::LocalCoordinates);
            CSTCoordinates::parse_local_coordinate(parser);
            parser.expect_inline_whitespace();
            CSTCoordinates::parse_local_coordinate(parser);
            parser.expect_inline_whitespace();
            CSTCoordinates::parse_local_coordinate(parser);
            parser.finish_node();

            true
        } else {
            false
        }
    }

    pub fn cast(node: &'a CSTNodeType) -> Option<CSTCoordinates<'a>> {
        match node.kind()? {
            SyntaxKind::WorldCoordinates => {
                CSTWorldCoordinates::cast(node).map(CSTCoordinates::World)
            }
            SyntaxKind::LocalCoordinates => {
                CSTLocalCoordinates::cast(node).map(CSTCoordinates::Local)
            }
            _ => {
                #[cfg(debug_assertions)]
                println!("Failed to cast node {:?} to CSTCoordinates", node);

                None
            }
        }
    }

    pub fn lower(self) -> Option<Coordinates> {
        Some(match self {
            CSTCoordinates::World(coordinates) => {
                let (x, y, z) = coordinates.coordinates()?;

                Coordinates::World(x.lower()?, y.lower()?, z.lower()?)
            }
            CSTCoordinates::Local(coordinates) => {
                let (x, y, z) = coordinates.coordinates();

                Coordinates::Local(x, y, z)
            }
        })
    }
}
