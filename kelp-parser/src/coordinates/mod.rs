use kelp_core::parsed::{
    coordinate::ParsedCoordinates, supports_expression_sigil::ParsedSupportsExpressionSigil,
};

use crate::{
    coordinates::{local::parse_local_coordinate, world::try_parse_world_coordinate},
    cst::{CSTActualCoordinates, CSTCoordinates, CSTExpressionSigil},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod local;
pub mod world;

impl ParsableAstNode for CSTCoordinates {
    fn try_parse(parser: &mut Parser) -> bool {
        if CSTExpressionSigil::try_parse(parser) {
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
                state.restore(parser);
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
}

impl LowerableAstNode for CSTActualCoordinates {
    type Lowered = ParsedCoordinates;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::WorldCoordinates(node) => node.lower(ctx),
            Self::LocalCoordinates(node) => node.lower(ctx),
        }
    }
}

impl LowerableAstNode for CSTCoordinates {
    type Lowered = ParsedSupportsExpressionSigil<ParsedCoordinates>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ActualCoordinates(node) => {
                node.lower(ctx).map(ParsedSupportsExpressionSigil::Regular)
            }
            Self::ExpressionSigil(node) => node
                .lower(ctx)
                .map(|lowered| lowered.retype_sigil().unwrap()),
        }
    }
}
