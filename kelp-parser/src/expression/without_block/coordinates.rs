use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTCoordinates, CSTCoordinatesExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTCoordinatesExpression {
    fn try_parse(parser: &mut Parser) -> bool {
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
}

impl LowerableAstNode for CSTCoordinatesExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let coordinates = self.coordinates()?.lower(ctx)?;

        Some(ParsedExpressionKind::Coordinates(Box::new(coordinates)).with_span(self.span()))
    }
}
