use std::collections::HashMap;

use nonempty::NonEmpty;

use crate::{
    high::{
        expression::Expression, semantic_analysis_context::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    middle::nbt_path::{NbtPath as MiddleNbtPath, NbtPathNode as MiddleNbtPathNode},
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum NbtPathNode {
    RootCompound(HashMap<SNBTString, Expression>),
    Named(SNBTString, Option<HashMap<SNBTString, Expression>>),
    Index(Option<Box<Expression>>),
}

impl NbtPathNode {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPathNode> {
        Some(match self {
            Self::RootCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                MiddleNbtPathNode::RootCompound(compound)
            }
            Self::Named(name, compound) => {
                let (_, name) = name.perform_semantic_analysis(ctx);
                let compound = match compound {
                    Some(compound) => Some(
                        compound
                            .into_iter()
                            .map(|(key, value)| {
                                let (_, key) = key.perform_semantic_analysis(ctx);
                                let (_, value) = value.perform_semantic_analysis(ctx)?;

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
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                MiddleNbtPathNode::Index(expression.map(Box::new))
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct NbtPath(pub NonEmpty<NbtPathNode>);

impl NbtPath {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPath> {
        Some(MiddleNbtPath(
            NonEmpty::from_vec(
                self.0
                    .into_iter()
                    .map(|node| node.perform_semantic_analysis(ctx))
                    .collect_option_all()?,
            )
            .unwrap(),
        ))
    }

    #[must_use]
    pub fn with_node(mut self, node: NbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
