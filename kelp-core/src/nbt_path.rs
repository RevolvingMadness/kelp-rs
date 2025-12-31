use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode, SNBTCompound};
use minecraft_command_types_derive::HasMacro;
use nonempty::NonEmpty;

use crate::{
    command::context::CompileContext,
    datapack::HighDatapack,
    expression::{Expression, ExpressionCompoundKind, StringExpression},
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighNbtPathNode {
    RootCompound(ExpressionCompoundKind),
    Named(StringExpression, Option<ExpressionCompoundKind>),
    Index(Option<Box<Expression>>),
}

impl HighNbtPathNode {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> NbtPathNode {
        match self {
            HighNbtPathNode::RootCompound(compound) => NbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);
                        let key = key.compile(datapack, ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            HighNbtPathNode::Named(name, expression) => {
                let name = name.compile(datapack, ctx);

                NbtPathNode::Named(
                    name,
                    expression.map(|expression| {
                        expression
                            .into_iter()
                            .map(|(key, value)| {
                                let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);
                                let key = key.compile(datapack, ctx);

                                (key, value)
                            })
                            .collect::<SNBTCompound>()
                    }),
                )
            }
            HighNbtPathNode::Index(expression) => NbtPathNode::Index(
                expression
                    .map(|expression| expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx)),
            ),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighNbtPath(pub NonEmpty<HighNbtPathNode>);

impl HighNbtPath {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> NbtPath {
        NbtPath(self.0.map(|node| node.compile(datapack, ctx)))
    }

    pub fn with_node(mut self, node: HighNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
