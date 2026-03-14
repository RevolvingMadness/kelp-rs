use ordered_float::NotNan;

use crate::{cst::CSTLocalCoordinate, parser::Parser, syntax::SyntaxKind};

pub fn parse_local_coordinate(parser: &mut Parser) {
    parser.start_node(SyntaxKind::LocalCoordinate);

    if !parser.expect_char('^', "Expected '^'") {
        parser.bump_char();
    }

    parser.try_parse_fractional_value();

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_local_coordinate(node: CSTLocalCoordinate) -> Option<Option<NotNan<f32>>> {
    let value = match node
        .fractional_value_token()
        .map(|token| token.text().parse::<NotNan<f32>>().ok())
    {
        None => None,
        Some(None) => return None,
        Some(Some(value)) => Some(value),
    };

    Some(value)
}
