use std::collections::BTreeMap;

use minecraft_command_types::{
    nbt_path::{NbtPath as LowNbtPath, NbtPathNode as LowNbtPathNode, SNBTCompound},
    snbt::SNBTString,
};
use minecraft_command_types_derive::HasMacro;
use nonempty::NonEmpty;

use crate::{compile_context::CompileContext, datapack::Datapack, middle::expression::Expression};

#[derive(Debug, Clone, HasMacro)]
pub enum NbtPathNode {
    RootCompound(BTreeMap<SNBTString, Expression>),
    Named(SNBTString, Option<BTreeMap<SNBTString, Expression>>),
    Index(Option<Box<Expression>>),
}

impl NbtPathNode {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowNbtPathNode {
        match self {
            Self::RootCompound(compound) => LowNbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            Self::Named(name, expression) => LowNbtPathNode::Named(
                name,
                expression.map(|expression| {
                    expression
                        .into_iter()
                        .map(|(key, value)| {
                            let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                            (key, value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
            Self::Index(expression) => LowNbtPathNode::Index(
                expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
        }
    }
}

#[derive(Debug, Clone, HasMacro)]
pub struct NbtPath(pub NonEmpty<NbtPathNode>);

impl NbtPath {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowNbtPath {
        LowNbtPath(self.0.map(|node| node.compile(datapack, ctx)))
    }

    #[must_use]
    pub fn with_node(mut self, node: NbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
