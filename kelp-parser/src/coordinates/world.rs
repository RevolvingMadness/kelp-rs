use kelp_core::high::{
    coordinate::WorldCoordinate, semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTWorldCoordinate,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_world_coordinate(parser: &mut Parser) {
    parser.start_node(SyntaxKind::WorldCoordinate);

    let Some(char) = parser.peek_char() else {
        parser.finish_node();

        return;
    };

    let has_symbol = char == '~' || char == '^';

    if !has_symbol && !char.is_numeric() {
        parser.finish_node();

        return;
    }

    if has_symbol {
        parser.bump_char();
    }

    try_parse_expression(parser);

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_world_coordinate(
    node: CSTWorldCoordinate,
    ctx: &mut SemanticAnalysisContext,
) -> Option<WorldCoordinate> {
    let is_relative = node.tilde_token().is_some();

    let value = node
        .expression()
        .and_then(|expression| lower_expression(expression, ctx));

    Some(WorldCoordinate {
        relative: is_relative,
        value,
    })
}
