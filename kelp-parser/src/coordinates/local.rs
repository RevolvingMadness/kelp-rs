use kelp_core::parsed::coordinate::{ParsedCoordinates, ParsedLocalCoordinate};

use crate::{
    cst::{CSTLocalCoordinate, CSTLocalCoordinates},
    expression::{lower_expression, try_parse_expression},
    extension_traits::LowerableAstNode,
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_local_coordinate(parser: &mut Parser) {
    parser.start_node(SyntaxKind::LocalCoordinate);

    if !parser.try_bump_char('^') {
        parser.error("Expected '^'");
    }

    try_parse_expression(parser);

    parser.finish_node();
}

impl LowerableAstNode for CSTLocalCoordinate {
    type Lowered = ParsedLocalCoordinate;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let result = self
            .expression()
            .map(|expression| lower_expression(expression, ctx));

        match result {
            Some(Some(expression)) => Some(Some(expression)),
            Some(None) => None,
            None => Some(None),
        }
    }
}

impl LowerableAstNode for CSTLocalCoordinates {
    type Lowered = ParsedCoordinates;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let mut coordinates = self
            .local_coordinates()
            .map(|coordinate| coordinate.lower(ctx));

        let x = coordinates.next().unwrap()?;
        let y = coordinates.next().unwrap()?;
        let z = coordinates.next().unwrap()?;

        Some(ParsedCoordinates::Local(x, y, z))
    }
}
