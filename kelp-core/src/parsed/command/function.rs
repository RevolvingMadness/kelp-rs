use std::collections::HashMap;

use crate::{
    parsed::{
        data::ParsedDataTarget, expression::ParsedExpression, nbt_path::NbtPath,
        semantic_analysis::SemanticAnalysisContext, snbt_string::SpannedSNBTString,
    },
    semantic::expression::command::function::SemanticFunctionCommandArguments,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum FunctionCommandArguments {
    Compound(HashMap<SpannedSNBTString, ParsedExpression>),
    DataTarget(Box<(ParsedDataTarget, Option<NbtPath>)>),
}

impl FunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticFunctionCommandArguments> {
        Some(match self {
            Self::Compound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let value = value.perform_semantic_analysis(ctx);

                        let (_, value) = value?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                SemanticFunctionCommandArguments::Compound(compound)
            }
            Self::DataTarget(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;

                SemanticFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
