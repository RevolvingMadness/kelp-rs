use kelp_core::parsed::nbt_path::NbtPath;

use crate::{
    cst::{CSTIndexNBTPathNode, CSTNBTPath, CSTNamedNBTPathNode},
    data::nbt_path::node::try_parse_start_nbt_path_node,
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod node;

pub fn try_parse_nbt_path(parser: &mut Parser) -> bool {
    let state = parser.save_state();
    parser.start_node(SyntaxKind::NBTPath);

    if !try_parse_start_nbt_path_node(parser) {
        state.restore(parser);

        return false;
    }

    loop {
        let before_rest = parser.pos;

        let has_dot = parser.try_bump_char('.');

        if has_dot {
            if !CSTNamedNBTPathNode::try_parse(parser) && !CSTIndexNBTPathNode::try_parse(parser) {
                parser.error("Expected path after '.'");

                break;
            }
        } else if !CSTIndexNBTPathNode::try_parse(parser) {
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
pub fn lower_nbt_path(node: CSTNBTPath, ctx: &mut LowerContext) -> Option<NbtPath> {
    let nodes = node
        .nbt_path_nodes()
        .filter_map(|nbt_path_node| nbt_path_node.lower(ctx))
        .collect();

    Some(NbtPath(nodes))
}
