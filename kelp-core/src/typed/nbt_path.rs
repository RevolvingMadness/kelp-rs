use std::collections::HashMap;

use minecraft_command_types::{
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    snbt::SNBTString,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub enum TypedNbtPathNode {
    RootCompound(HashMap<String, TypedExpressionId>),
    Named(String, Option<HashMap<String, TypedExpressionId>>),
    Index(Option<TypedExpressionId>),
}

impl TypedNbtPathNode {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> NbtPathNode {
        match self {
            Self::RootCompound(compound) => NbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = TypedExpression::resolve(value, allocator, datapack, ctx)
                            .as_snbt_macros(ctx);

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
                            let value = TypedExpression::resolve(value, allocator, datapack, ctx)
                                .as_snbt_macros(ctx);

                            (SNBTString(false, key), value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
            Self::Index(expression) => NbtPathNode::Index(expression.map(|expression| {
                TypedExpression::resolve(expression, allocator, datapack, ctx).as_snbt_macros(ctx)
            })),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedNbtPath(pub Vec<TypedNbtPathNode>);

impl TypedNbtPath {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> NbtPath {
        NbtPath(
            self.0
                .into_iter()
                .map(|node| node.compile(allocator, datapack, ctx))
                .collect(),
        )
    }

    #[must_use]
    pub fn with_node(mut self, node: TypedNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
