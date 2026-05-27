use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::CSTPathDataType,
    parser::Parser,
    path::generic::{lower_generic_path, try_parse_generic_path},
    syntax::SyntaxKind,
};

pub fn try_parse_path_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_generic_path(parser, true) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::PathDataType);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_data_type(node: CSTPathDataType) -> Option<ParsedDataType> {
    let path = lower_generic_path(node.generic_path()?)?;

    Some(ParsedDataType::Named(path))
}
