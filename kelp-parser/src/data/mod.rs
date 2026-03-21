use kelp_core::high::{
    data::DataTarget, nbt_path::NbtPath, semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTData,
    data::{
        nbt_path::{lower_nbt_path, try_parse_nbt_path},
        target::{lower_data_target, try_parse_data_target},
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod nbt_path;
pub mod target;

#[must_use]
pub fn try_parse_data(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_data_target(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::Data);

    parser.expect_inline_whitespace();

    if !try_parse_nbt_path(parser) {
        parser.error("Expected nbt path");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_data(
    node: CSTData,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(DataTarget, NbtPath)> {
    let target = lower_data_target(node.data_target()?)?;
    let path = lower_nbt_path(node.n_b_t_path()?, ctx)?;

    Some((target, path))
}
