use std::collections::HashMap;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        data::DataTarget,
        expression::{ParsedExpression, ParsedExpressionId},
        nbt_path::ParsedNbtPath,
        semantic_analysis::SemanticAnalysisContext,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::expression::command::function::TypedFunctionCommandArguments,
};

#[derive(Debug, Clone)]
pub enum ParsedFunctionCommandArguments {
    Compound(HashMap<String, ParsedExpressionId>),
    DataTarget(Box<(DataTarget, Option<ParsedNbtPath>)>),
}

impl ParsedFunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedFunctionCommandArguments> {
        Some(match self {
            Self::Compound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = ParsedExpression::perform_semantic_analysis(
                            value,
                            parsed_arena,
                            typed_arena,
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

                let target = target.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let path = match path {
                    Some(path) => {
                        Some(path.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                let target = target?;

                TypedFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
