use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode, SNBTCompound};
use minecraft_command_types_derive::HasMacro;
use nonempty::NonEmpty;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::{Expression, ExpressionCompoundKind},
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionUnitIterExt,
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
            Self::RootCompound(compound) => compound
                .iter()
                .map(|(key, value)| {
                    let key = key.perform_semantic_analysis(ctx, is_lhs);
                    let value = value.perform_semantic_analysis(ctx, is_lhs, None);

                    key?;
                    value?;

                    Some(())
                })
                .all_some(),
            Self::Named(name, compound) => {
                let name = name.perform_semantic_analysis(ctx, is_lhs);
                let compound = compound.as_ref().map_or(Some(()), |compound| {
                    compound
                        .iter()
                        .map(|(key, value)| {
                            let key = key.perform_semantic_analysis(ctx, is_lhs);
                            let value = value.perform_semantic_analysis(ctx, is_lhs, None);

                            key?;
                            value?;

                            Some(())
                        })
                        .collect::<Option<()>>()
                });

                name?;
                compound?;

                Some(())
            }
            Self::Index(expression) => expression.as_ref().map_or(Some(()), |expression| {
                expression.perform_semantic_analysis(ctx, is_lhs, None)
            }),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> NbtPathNode {
        match self {
            Self::RootCompound(compound) => NbtPathNode::RootCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect(),
            ),
            Self::Named(name, expression) => NbtPathNode::Named(
                name.snbt_string,
                expression.map(|expression| {
                    expression
                        .into_iter()
                        .map(|(key, value)| {
                            let value = value.resolve(datapack, ctx).as_snbt_macros(ctx);

                            (key.snbt_string, value)
                        })
                        .collect::<SNBTCompound>()
                }),
            ),
            Self::Index(expression) => NbtPathNode::Index(
                expression.map(|expression| expression.resolve(datapack, ctx).as_snbt_macros(ctx)),
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

    #[must_use]
    pub fn with_node(mut self, node: HighNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
