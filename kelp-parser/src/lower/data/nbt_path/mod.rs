use kelp_core::high::nbt_path::HighNbtPath;
use nonempty::NonEmpty;

use crate::{
    cst_node,
    lower::data::nbt_path::node::{
        CSTNBTPathNode, index::CSTNBTPathIndexNode, named::CSTNBTPathNamedNode,
    },
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod node;

cst_node!(CSTNBTPath, SyntaxKind::NBTPath);

impl<'a> CSTNBTPath<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        parser.start_node(SyntaxKind::NBTPath);

        if !CSTNBTPathNode::try_parse_start(parser) {
            parser.restore_state((parser.pos, checkpoint));

            return false;
        }

        loop {
            let before_rest = parser.pos;

            let has_dot = parser.try_bump_char('.');

            if has_dot {
                if !CSTNBTPathNamedNode::try_parse(parser)
                    && !CSTNBTPathIndexNode::try_parse(parser)
                {
                    parser.error("Expected path after '.'");

                    break;
                }
            } else if !CSTNBTPathIndexNode::try_parse(parser) {
                break;
            }

            if parser.pos == before_rest {
                break;
            }
        }

        parser.finish_node();

        true
    }

    pub fn nodes(&self) -> Vec<CSTNBTPathNode<'a>> {
        self.children().filter_map(CSTNBTPathNode::cast).collect()
    }

    pub fn lower(self, text: &str) -> Option<HighNbtPath> {
        let nodes = self
            .nodes()
            .into_iter()
            .filter_map(|node| node.lower(text))
            .collect();

        Some(HighNbtPath(NonEmpty::from_vec(nodes)?))
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        for node in self.nodes() {
            node.collect_semantic_tokens(tokens);
        }
    }
}
