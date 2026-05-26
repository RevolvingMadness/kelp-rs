use std::collections::HashMap;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        data::DataTarget,
        expression::{ParsedExpression, ParsedExpressionId},
        nbt_path::ParsedNbtPath,
        semantic_analysis::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::expression::command::function::TypedFunctionCommandArguments,
};

#[derive(Debug, Clone)]
pub enum ParsedFunctionCommandArguments {
    Compound(HashMap<SNBTString, ParsedExpressionId>),
    DataTarget(Box<(DataTarget, Option<ParsedNbtPath>)>),
}

impl ParsedFunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedFunctionCommandArguments> {
        Some(match self {
            Self::Compound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let value = ParsedExpression::perform_semantic_analysis(
                            value,
                            high_allocator,
                            low_allocator,
                            ctx,
                        );

                        let value = value?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                TypedFunctionCommandArguments::Compound(compound)
            }
            Self::DataTarget(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = match path {
                    Some(path) => {
                        Some(path.perform_semantic_analysis(high_allocator, low_allocator, ctx)?)
                    }
                    None => None,
                };

                let target = target?;

                TypedFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
