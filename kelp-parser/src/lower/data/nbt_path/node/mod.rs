use kelp_core::high::{nbt_path::HighNbtPathNode, snbt_string::HighSNBTString};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cstlib::CSTNodeType,
    lower::{
        data::nbt_path::node::{index::CSTNBTPathIndexNode, named::CSTNBTPathNamedNode},
        expression::{CSTExpression, compound::CSTCompoundExpression},
    },
    parser::Parser,
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
    pub(crate) fn try_parse_start(parser: &mut Parser) -> bool {
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

    pub fn cast(node: &'a CSTNodeType) -> Option<CSTNBTPathNode<'a>> {
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

    pub fn lower(node: CSTNBTPathNode) -> Option<HighNbtPathNode> {
        Some(match node {
            CSTNBTPathNode::Index(node) => {
                let index = node.index();

                HighNbtPathNode::Index(index.and_then(CSTExpression::lower).map(Box::new))
            }
            CSTNBTPathNode::Named(node) => {
                let (span, name) = node.name()?;
                let compound = node.compound().map(CSTCompoundExpression::lower);

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
}
