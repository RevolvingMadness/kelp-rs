use std::collections::HashMap;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::nbt_path::{TypedNbtPath as MiddleNbtPath, TypedNbtPathNode as MiddleNbtPathNode},
};

#[derive(Debug, Clone)]
pub enum ParsedNbtPathNode {
    RootCompound(HashMap<String, ParsedExpressionId>),
    Named(String, Option<HashMap<String, ParsedExpressionId>>),
    Index(Option<ParsedExpressionId>),
}

impl ParsedNbtPathNode {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPathNode> {
        Some(match self {
            Self::RootCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = ParsedExpression::perform_semantic_analysis(
                            value,
                            parsed_arena,
                            typed_arena,
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
                                let value = ParsedExpression::perform_semantic_analysis(
                                    value,
                                    parsed_arena,
                                    typed_arena,
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
                        let expression = ParsedExpression::perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
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
pub struct ParsedNbtPath(pub Vec<ParsedNbtPathNode>);

impl ParsedNbtPath {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleNbtPath> {
        Some(MiddleNbtPath(
            self.0
                .into_iter()
                .map(|node| node.perform_semantic_analysis(parsed_arena, typed_arena, ctx))
                .collect_option_all()?,
        ))
    }

    #[must_use]
    pub fn with_node(mut self, node: ParsedNbtPathNode) -> Self {
        self.0.push(node);

        self
    }
}
