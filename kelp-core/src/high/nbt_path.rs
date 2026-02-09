use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode, SNBTCompound};
use minecraft_command_types_derive::HasMacro;
use nonempty::NonEmpty;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::{Expression, ExpressionCompoundKind},
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighNbtPathNode {
    RootCompound(ExpressionCompoundKind),
    Named(HighSNBTString, Option<ExpressionCompoundKind>),
    Index(Option<Box<Expression>>),
}

impl HighNbtPathNode {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighNbtPathNode::RootCompound(compound) => compound
                .iter()
                .map(|(key, value)| {
                    let key = key.perform_semantic_analysis(ctx, is_lhs);
                    let value = value.perform_semantic_analysis(ctx, is_lhs);

                    key?;
                    value?;

                    Some(())
                })
                .all_some(),
            HighNbtPathNode::Named(name, compound) => {
                let name = name.perform_semantic_analysis(ctx, is_lhs);
                let compound = compound
                    .as_ref()
                    .map(|compound| {
                        compound
                            .iter()
                            .map(|(key, value)| {
                                let key = key.perform_semantic_analysis(ctx, is_lhs);
                                let value = value.perform_semantic_analysis(ctx, is_lhs);

                                key?;
                                value?;

                                Some(())
                            })
                            .collect::<Option<()>>()
                    })
                    .unwrap_or(Some(()));

                name?;
                compound?;

                Some(())
            }
            HighNbtPathNode::Index(expression) => expression
                .as_ref()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .unwrap_or(Some(())),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> NbtPathNode {
        match self {
            HighNbtPathNode::RootCompound(compound) => NbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect(),
            ),
            HighNbtPathNode::Named(name, expression) => NbtPathNode::Named(
                name.snbt_string,
                expression.map(|expression| {
                    expression
                        .into_iter()
                        .map(|(key, value)| {
                            let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                            (key.snbt_string, value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
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
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        self.0
            .iter()
            .map(|node| node.perform_semantic_analysis(ctx, is_lhs))
            .all_some()
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> NbtPath {
        NbtPath(self.0.map(|node| node.compile(datapack, ctx)))
    }

    pub fn with_node(mut self, node: HighNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
