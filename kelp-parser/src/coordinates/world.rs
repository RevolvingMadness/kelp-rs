use kelp_core::parsed::coordinate::{ParsedCoordinates, ParsedWorldCoordinate};

use crate::{
    cst::{CSTExpression, CSTWorldCoordinate, CSTWorldCoordinates},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_world_coordinate(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::WorldCoordinate);

    let mut parsed = false;

    match parser.peek_char() {
        Some('~') => {
            parser.bump_char_kind(SyntaxKind::Tilde);

            parsed = true;
        }
        Some('^') => {
            parser.bump_char_kind(SyntaxKind::Caret);

            parsed = true;
        }
        _ => {}
    }

    if CSTExpression::try_parse(parser) {
        parsed = true;
    }

    parser.finish_node();

    parsed
}

impl LowerableAstNode for CSTWorldCoordinate {
    type Lowered = ParsedWorldCoordinate;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let is_relative = self.tilde_token().is_some() || self.caret_token().is_some();

        let value = self
            .expression()
            .and_then(|expression| expression.lower(ctx));

        if is_relative {
            Some(ParsedWorldCoordinate::Relative(value))
        } else {
            value.map(ParsedWorldCoordinate::Absolute)
        }
    }
}

impl LowerableAstNode for CSTWorldCoordinates {
    type Lowered = ParsedCoordinates;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let mut coordinates = self.world_coordinates();

        let x = coordinates.next()?.lower(ctx)?;
        let y = coordinates.next()?.lower(ctx)?;
        let z = coordinates.next()?.lower(ctx)?;

        Some(ParsedCoordinates::World(x, y, z))
    }
}
