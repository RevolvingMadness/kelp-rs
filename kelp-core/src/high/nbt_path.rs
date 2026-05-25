use std::collections::HashMap;

use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{expression::Expression, semantic_analysis::SemanticAnalysisContext},
    low::nbt_path::{NbtPath as MiddleNbtPath, NbtPathNode as MiddleNbtPathNode},
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum NbtPathNode {
    RootCompound(HashMap<String, Idx<Expression>>),
    Named(String, Option<HashMap<String, Idx<Expression>>>),
    Index(Option<Idx<Expression>>),
}

impl NbtPathNode {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPathNode> {
        Some(match self {
            Self::RootCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = Expression::perform_semantic_analysis(
                            value,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                MiddleNbtPathNode::RootCompound(compound)
            }
            Self::Named(name, compound) => {
                let compound = match compound {
                    Some(compound) => Some(
                        compound
                            .into_iter()
                            .map(|(key, value)| {
                                let value = Expression::perform_semantic_analysis(
                                    value,
                                    high_allocator,
                                    low_allocator,
                                    ctx,
                                )?;

                                Some((key, value))
                            })
                            .collect_option_all()?,
                    ),
                    None => None,
                };

                MiddleNbtPathNode::Named(name, compound)
            }
            Self::Index(expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let expression = Expression::perform_semantic_analysis(
                            expression,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                MiddleNbtPathNode::Index(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct NbtPath(pub Vec<NbtPathNode>);

impl NbtPath {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPath> {
        Some(MiddleNbtPath(
            self.0
                .into_iter()
                .map(|node| node.perform_semantic_analysis(high_allocator, low_allocator, ctx))
                .collect_option_all()?,
        ))
    }

    #[must_use]
    pub fn with_node(mut self, node: NbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
