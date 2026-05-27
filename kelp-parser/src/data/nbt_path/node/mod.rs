use crate::{
    cst::{CSTCompoundExpression, CSTIndexNBTPathNode, CSTNBTPathNode, CSTNamedNBTPathNode},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};
use kelp_core::parsed::nbt_path::NbtPathNode;

pub mod index;
pub mod named;

pub fn try_parse_start_nbt_path_node(parser: &mut Parser) -> bool {
    let Some(char) = parser.peek_char() else {
        return false;
    };

    match char {
        '{' => {
            parser.start_node(SyntaxKind::CompoundNBTPathNode);

            if !CSTCompoundExpression::try_parse(parser) {
                parser.error("Expected compound");
            }

            parser.finish_node();

            true
        }
        '[' => CSTIndexNBTPathNode::try_parse(parser),
        _ => CSTNamedNBTPathNode::try_parse(parser),
    }
}

impl LowerableAstNode for CSTNBTPathNode {
    type Lowered = NbtPathNode;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::IndexNBTPathNode(node) => {
                let index = node.index();

                Some(NbtPathNode::Index(
                    index
                        .and_then(|expression| expression.lower(ctx))
                        .map(Box::new),
                ))
            }
            Self::NamedNBTPathNode(node) => {
                let name_token = node.name()?;
                let name = name_token.text();

                let compound = node.compound().and_then(|compound| {
                    let (_, compound) = compound.lower(ctx)?;

                    Some(compound)
                });

                Some(NbtPathNode::Named(name.to_owned(), compound))
            }
            Self::CompoundNBTPathNode(node) => {
                let (_, compound) = node.compound_expression()?.lower(ctx)?;

                Some(NbtPathNode::RootCompound(compound))
            }
        }
    }
}
