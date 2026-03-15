use std::collections::BTreeMap;

use minecraft_command_types_derive::HasMacro;
use nonempty::NonEmpty;

use crate::{
    high::expression::Expression,
    high::snbt_string::SNBTString,
    middle::nbt_path::{NbtPath as MiddleNbtPath, NbtPathNode as MiddleNbtPathNode},
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum NbtPathNode {
    RootCompound(BTreeMap<SNBTString, Expression>),
    Named(SNBTString, Option<BTreeMap<SNBTString, Expression>>),
    Index(Option<Box<Expression>>),
}

impl NbtPathNode {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleNbtPathNode> {
        Some(match self {
            Self::RootCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx, is_lhs);
                        let (_, value) = value.perform_semantic_analysis(ctx, is_lhs)?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                MiddleNbtPathNode::RootCompound(compound)
            }
            Self::Named(name, compound) => {
                let (_, name) = name.perform_semantic_analysis(ctx, is_lhs);
                let compound = match compound {
                    Some(compound) => Some(
                        compound
                            .into_iter()
                            .map(|(key, value)| {
                                let (_, key) = key.perform_semantic_analysis(ctx, is_lhs);
                                let (_, value) = value.perform_semantic_analysis(ctx, is_lhs)?;

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
                        let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                        Some(expression)
                    }
                    None => None,
                };

                MiddleNbtPathNode::Index(expression.map(Box::new))
            }
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct NbtPath(pub NonEmpty<NbtPathNode>);

impl NbtPath {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleNbtPath> {
        Some(MiddleNbtPath(
            NonEmpty::from_vec(
                self.0
                    .into_iter()
                    .map(|node| node.perform_semantic_analysis(ctx, is_lhs))
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
