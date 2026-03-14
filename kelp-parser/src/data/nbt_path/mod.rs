use kelp_core::{high::nbt_path::HighNbtPath, semantic_analysis_context::SemanticAnalysisContext};
use nonempty::NonEmpty;

use crate::{
    cst::CSTNBTPath,
    data::nbt_path::node::{
        index::try_parse_index_nbt_path_node, lower_nbt_path_node,
        named::try_parse_named_nbt_path_node, try_parse_start_nbt_path_node,
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod node;

pub fn try_parse_nbt_path(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.start_node(SyntaxKind::NBTPath);

    if !try_parse_start_nbt_path_node(parser) {
        parser.restore_state((parser.pos, checkpoint));

        return false;
    }

    loop {
        let before_rest = parser.pos;

        let has_dot = parser.try_bump_char('.');

        if has_dot {
            if !try_parse_named_nbt_path_node(parser) && !try_parse_index_nbt_path_node(parser) {
                parser.error("Expected path after '.'");

                break;
            }
        } else if !try_parse_index_nbt_path_node(parser) {
            break;
        }

        if parser.pos == before_rest {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_nbt_path(node: CSTNBTPath, ctx: &mut SemanticAnalysisContext) -> Option<HighNbtPath> {
    let nodes = node
        .n_b_t_path_nodes()
        .filter_map(|nbt_path_node| lower_nbt_path_node(nbt_path_node, ctx))
        .collect();

    Some(HighNbtPath(NonEmpty::from_vec(nodes)?))
}
