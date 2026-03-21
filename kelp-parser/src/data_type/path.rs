use kelp_core::high::data_type::unresolved::UnresolvedDataType;

use crate::{
    cst::CSTPathDataType,
    parser::Parser,
    path::{lower_path, try_parse_path},
    syntax::SyntaxKind,
};

pub fn try_parse_path_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_path(parser, true) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::PathDataType);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_path_data_type(node: CSTPathDataType) -> Option<UnresolvedDataType> {
    let path = lower_path(node.path()?)?;

    Some(UnresolvedDataType::Named(path))
}
