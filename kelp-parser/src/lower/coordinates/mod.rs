use minecraft_command_types::coordinate::Coordinates;

use crate::{
    cstlib::CSTNodeType,
    lower::coordinates::{
        local::CSTLocalCoordinates,
        world::{CSTWorldCoordinate, CSTWorldCoordinates},
    },
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod local;
pub mod world;

pub enum CSTCoordinates<'a> {
    World(CSTWorldCoordinates<'a>),
    Local(CSTLocalCoordinates<'a>),
}

impl<'a> CSTCoordinates<'a> {
    pub fn parse_local_coordinate(parser: &mut Parser) {
        parser.start_node(SyntaxKind::LocalCoordinate);

        if !parser.expect_char('^', "Expected '^'") {
            parser.bump_char();
        }

        parser.try_parse_fractional_value();

        parser.finish_node();
    }

    pub fn try_parse(parser: &mut Parser) -> bool {
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

    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
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

    #[must_use]
    pub fn lower(self, text: &str) -> Option<Coordinates> {
        Some(match self {
            CSTCoordinates::World(coordinates) => {
                let (x, y, z) = coordinates.coordinates();

                Coordinates::World(x?.lower(text)?, y?.lower(text)?, z?.lower(text)?)
            }
            CSTCoordinates::Local(coordinates) => {
                let (x, y, z) = coordinates.coordinates();

                Coordinates::Local(
                    x.and_then(|x| x.lower(text)),
                    y.and_then(|y| y.lower(text)),
                    z.and_then(|z| z.lower(text)),
                )
            }
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match self {
            CSTCoordinates::World(coordinates) => {
                let (x, y, z) = coordinates.coordinates();

                if let Some(x) = x {
                    x.collect_semantic_tokens(tokens);
                }

                if let Some(y) = y {
                    y.collect_semantic_tokens(tokens);
                }

                if let Some(z) = z {
                    z.collect_semantic_tokens(tokens);
                }
            }
            CSTCoordinates::Local(coordinates) => {
                let (x, y, z) = coordinates.coordinates();

                if let Some(x) = x {
                    x.collect_semantic_tokens(tokens);
                }

                if let Some(y) = y {
                    y.collect_semantic_tokens(tokens);
                }

                if let Some(z) = z {
                    z.collect_semantic_tokens(tokens);
                }
            }
        }
    }
}
