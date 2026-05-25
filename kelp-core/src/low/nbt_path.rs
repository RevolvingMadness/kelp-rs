use std::collections::HashMap;

use la_arena::Idx;
use minecraft_command_types::{
    nbt_path::{NbtPath as LowNbtPath, NbtPathNode as LowNbtPathNode, SNBTCompound},
    snbt::SNBTString,
};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    low::expression::unresolved::UnresolvedExpression,
};

#[derive(Debug, Clone)]
pub enum NbtPathNode {
    RootCompound(HashMap<String, Idx<UnresolvedExpression>>),
    Named(String, Option<HashMap<String, Idx<UnresolvedExpression>>>),
    Index(Option<Idx<UnresolvedExpression>>),
}

impl NbtPathNode {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowNbtPathNode {
        match self {
            Self::RootCompound(compound) => LowNbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = UnresolvedExpression::resolve(value, allocator, datapack, ctx)
                            .as_snbt_macros(ctx);

                        (SNBTString(false, key), value)
                    })
                    .collect(),
            ),
            Self::Named(name, expression) => LowNbtPathNode::Named(
                SNBTString(false, name),
                expression.map(|expression| {
                    expression
                        .into_iter()
                        .map(|(key, value)| {
                            let value =
                                UnresolvedExpression::resolve(value, allocator, datapack, ctx)
                                    .as_snbt_macros(ctx);

                            (SNBTString(false, key), value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
            Self::Index(expression) => LowNbtPathNode::Index(expression.map(|expression| {
                UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx)
            })),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NbtPath(pub Vec<NbtPathNode>);

impl NbtPath {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowNbtPath {
        LowNbtPath(
            self.0
                .into_iter()
                .map(|node| node.compile(allocator, datapack, ctx))
                .collect(),
        )
    }

    #[must_use]
    pub fn with_node(mut self, node: NbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
