use std::collections::HashMap;

use crate::{
    parsed::{expression::ParsedExpression, semantic_analysis::SemanticAnalysisContext},
    semantic::nbt_path::{
        SemanticNbtPath as MiddleNbtPath, SemanticNbtPathNode as MiddleNbtPathNode,
    },
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum NbtPathNode {
    RootCompound(HashMap<String, ParsedExpression>),
    Named(String, Option<HashMap<String, ParsedExpression>>),
    Index(Option<Box<ParsedExpression>>),
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
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

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
pub struct NbtPath(pub Vec<NbtPathNode>);

impl NbtPath {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPath> {
        Some(MiddleNbtPath(
            self.0
                .into_iter()
                .map(|node| node.perform_semantic_analysis(ctx))
                .collect_option_all()?,
        ))
    }

    #[must_use]
    pub fn with_node(mut self, node: NbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
