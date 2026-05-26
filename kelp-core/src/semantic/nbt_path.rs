use std::collections::HashMap;

use minecraft_command_types::{
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    snbt::SNBTString,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, semantic::expression::SemanticExpression,
};

#[derive(Debug, Clone)]
pub enum SemanticNbtPathNode {
    RootCompound(HashMap<String, SemanticExpression>),
    Named(String, Option<HashMap<String, SemanticExpression>>),
    Index(Option<Box<SemanticExpression>>),
}

impl SemanticNbtPathNode {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> NbtPathNode {
        match self {
            Self::RootCompound(compound) => NbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (SNBTString(false, key), value)
                    })
                    .collect(),
            ),
            Self::Named(name, expression) => NbtPathNode::Named(
                SNBTString(false, name),
                expression.map(|expression| {
                    expression
                        .into_iter()
                        .map(|(key, value)| {
                            let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                            (SNBTString(false, key), value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
            Self::Index(expression) => NbtPathNode::Index(
                expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticNbtPath(pub Vec<SemanticNbtPathNode>);

impl SemanticNbtPath {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> NbtPath {
        NbtPath(
            self.0
                .into_iter()
                .map(|node| node.compile(datapack, ctx))
                .collect(),
        )
    }

    #[must_use]
    pub fn with_node(mut self, node: SemanticNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
