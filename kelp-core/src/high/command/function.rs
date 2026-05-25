use std::collections::HashMap;

use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        data::DataTarget, expression::Expression, nbt_path::NbtPath,
        semantic_analysis::SemanticAnalysisContext, snbt_string::SNBTString,
    },
    low::expression::command::function::FunctionCommandArguments as MiddleFunctionCommandArguments,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum FunctionCommandArguments {
    Compound(HashMap<SNBTString, Idx<Expression>>),
    DataTarget(Box<(DataTarget, Option<NbtPath>)>),
}

impl FunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleFunctionCommandArguments> {
        Some(match self {
            Self::Compound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let value = Expression::perform_semantic_analysis(
                            value,
                            high_allocator,
                            low_allocator,
                            ctx,
                        );

                        let value = value?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                MiddleFunctionCommandArguments::Compound(compound)
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

                MiddleFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
