use kelp_core::high::{nbt_path::HighNbtPathNode, snbt_string::HighSNBTString};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cstlib::CSTNodeType,
    lower::{
        data::nbt_path::node::{index::CSTNBTPathIndexNode, named::CSTNBTPathNamedNode},
        expression::compound::CSTCompoundExpression,
    },
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod index;
pub mod named;

#[derive(Debug)]
pub enum CSTNBTPathNode<'a> {
    Named(CSTNBTPathNamedNode<'a>),
    Index(CSTNBTPathIndexNode<'a>),
}

impl<'a> CSTNBTPathNode<'a> {
    pub fn try_parse_start(parser: &mut Parser) -> bool {
        let char = parser.peek_char();

        if char == Some('{') {
            parser.start_node(SyntaxKind::NBTPathRoot);
            let ok = CSTCompoundExpression::try_parse(parser);
            parser.finish_node();
            return ok;
        }

        if char == Some('[') {
            return CSTNBTPathIndexNode::try_parse(parser);
        }

        CSTNBTPathNamedNode::try_parse(parser)
    }

    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
        match node.kind()? {
            SyntaxKind::NBTPathNamed => CSTNBTPathNamedNode::cast(node).map(CSTNBTPathNode::Named),
            SyntaxKind::NBTPathIndex => CSTNBTPathIndexNode::cast(node).map(CSTNBTPathNode::Index),
            _ => {
                #[cfg(debug_assertions)]
                println!("Failed to cast node {:?} to CSTNBTPathNode", node);

                None
            }
        }
    }

    pub fn lower(self, text: &str) -> Option<HighNbtPathNode> {
        Some(match self {
            CSTNBTPathNode::Index(node) => {
                let index = node.index();

                HighNbtPathNode::Index(
                    index
                        .and_then(|expression| expression.lower(text))
                        .map(Box::new),
                )
            }
            CSTNBTPathNode::Named(node) => {
                let (span, name) = node.name(text)?;
                let compound = node
                    .compound()
                    .map(|compound_expression| compound_expression.lower(text));

                HighNbtPathNode::Named(
                    HighSNBTString {
                        snbt_string: SNBTString(false, name.to_string()),
                        span,
                    },
                    compound,
                )
            }
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match self {
            CSTNBTPathNode::Named(node) => {
                node.collect_semantic_tokens(tokens);
            }
            CSTNBTPathNode::Index(node) => {
                node.collect_semantic_tokens(tokens);
            }
        }
    }
}
