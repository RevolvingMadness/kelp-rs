use kelp_core::{parsed::nbt_path::NbtPath, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{CSTIndexNBTPathNode, CSTNBTPath, CSTNamedNBTPathNode},
    data::nbt_path::node::try_parse_start_nbt_path_node,
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod node;

impl ParsableAstNode for CSTNBTPath {
    fn try_parse(parser: &mut Parser) -> bool {
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
                if !CSTNamedNBTPathNode::try_parse(parser)
                    && !CSTIndexNBTPathNode::try_parse(parser)
                {
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
}

impl LowerableAstNode for CSTNBTPath {
    type Lowered = NbtPath;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let nodes = self
            .nbt_path_nodes()
            .map(|node| node.lower(ctx))
            .collect_option_all()?;

        Some(NbtPath(nodes))
    }
}
