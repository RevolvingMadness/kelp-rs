use crate::{
    cst::CSTNBTPathNode,
    lower::{
        data::nbt_path::node::{
            index::try_parse_index_nbt_path_node, named::try_parse_named_nbt_path_node,
        },
        expression::{
            compound::{lower_compound_expression_inner, try_parse_compound_expression},
            lower_expression,
        },
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};
use kelp_core::high::{nbt_path::HighNbtPathNode, snbt_string::HighSNBTString};
use minecraft_command_types::snbt::SNBTString;

pub mod index;
pub mod named;

pub fn try_parse_start_nbt_path_node(parser: &mut Parser) -> bool {
    let Some(char) = parser.peek_char() else {
        return false;
    };

    match char {
        '{' => {
            parser.start_node(SyntaxKind::CompoundNBTPathNode);

            if !try_parse_compound_expression(parser) {
                parser.error("Expected compound");
            }

            parser.finish_node();

            true
        }
        '[' => try_parse_index_nbt_path_node(parser),
        _ => try_parse_named_nbt_path_node(parser),
    }
}

pub fn lower_nbt_path_node(node: CSTNBTPathNode) -> Option<HighNbtPathNode> {
    match node {
        CSTNBTPathNode::IndexNBTPathNode(node) => {
            let index = node.index();

            Some(HighNbtPathNode::Index(
                index.and_then(lower_expression).map(Box::new),
            ))
        }
        CSTNBTPathNode::NamedNBTPathNode(node) => {
            let span = span_of_cst_node(&node);

            let name_token = node.name()?;
            let name = name_token.text();

            let compound = node.compound().and_then(|compound| {
                let (_, compound) = lower_compound_expression_inner(compound)?;

                Some(compound)
            });

            Some(HighNbtPathNode::Named(
                HighSNBTString {
                    snbt_string: SNBTString(false, name.to_string()),
                    span,
                },
                compound,
            ))
        }
        CSTNBTPathNode::CompoundNBTPathNode(node) => {
            let (_, compound) = lower_compound_expression_inner(node.compound_expression()?)?;

            Some(HighNbtPathNode::RootCompound(compound))
        }
    }
}
