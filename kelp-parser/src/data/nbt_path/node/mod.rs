use crate::{
    cst::CSTNBTPathNode,
    data::nbt_path::node::{
        index::try_parse_index_nbt_path_node, named::try_parse_named_nbt_path_node,
    },
    expression::{
        lower_expression,
        without_block::compound::{lower_compound_expression_inner, try_parse_compound_expression},
    },
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};
use kelp_core::parsed::nbt_path::ParsedNbtPathNode;

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

pub fn lower_nbt_path_node(
    node: CSTNBTPathNode,
    ctx: &mut LowerContext,
) -> Option<ParsedNbtPathNode> {
    match node {
        CSTNBTPathNode::IndexNBTPathNode(node) => {
            let index = node.index();

            Some(ParsedNbtPathNode::Index(
                index.and_then(|expression| lower_expression(expression, ctx)),
            ))
        }
        CSTNBTPathNode::NamedNBTPathNode(node) => {
            let name_token = node.name()?;
            let name = name_token.text();

            let compound = node.compound().and_then(|compound| {
                let (_, compound) = lower_compound_expression_inner(compound, ctx)?;

                Some(compound)
            });

            Some(ParsedNbtPathNode::Named(name.to_owned(), compound))
        }
        CSTNBTPathNode::CompoundNBTPathNode(node) => {
            let (_, compound) = lower_compound_expression_inner(node.compound_expression()?, ctx)?;

            Some(ParsedNbtPathNode::RootCompound(compound))
        }
    }
}
