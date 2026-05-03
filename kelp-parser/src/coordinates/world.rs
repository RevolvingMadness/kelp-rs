use kelp_core::high::{coordinate::WorldCoordinate, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTWorldCoordinate,
    expression::{lower_expression, try_parse_expression},
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

    if try_parse_expression(parser) {
        parsed = true;
    }

    parser.finish_node();

    parsed
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_world_coordinate(
    node: CSTWorldCoordinate,
    ctx: &mut SemanticAnalysisContext,
) -> Option<WorldCoordinate> {
    let is_relative = node.tilde_token().is_some() || node.caret_token().is_some();

    let value = node
        .expression()
        .and_then(|expression| lower_expression(expression, ctx));

    if is_relative {
        Some(WorldCoordinate::Relative(value))
    } else {
        value.map(WorldCoordinate::Absolute)
    }
}
