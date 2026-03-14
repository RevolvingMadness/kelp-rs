use minecraft_command_types::coordinate::WorldCoordinate;
use ordered_float::NotNan;

use crate::{cst::CSTWorldCoordinate, parser::Parser, syntax::SyntaxKind};

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

    parser.try_parse_fractional_value();
    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_world_coordinate(node: CSTWorldCoordinate) -> Option<WorldCoordinate> {
    let is_relative = node.tilde_token().is_some();

    let value = match node
        .fractional_value_token()
        .map(|token| token.text().parse::<NotNan<f32>>().ok())
    {
        None => None,
        Some(None) => return None,
        Some(Some(value)) => Some(value),
    };

    Some(WorldCoordinate {
        relative: is_relative,
        value,
    })
}
