use std::collections::BTreeMap;

use crate::{
    high::{
        data::DataTarget, expression::Expression, nbt_path::NbtPath,
        semantic_analysis_context::SemanticAnalysisContext, snbt_string::SNBTString,
    },
    middle::expression::command::function::FunctionCommandArguments as MiddleFunctionCommandArguments,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum FunctionCommandArguments {
    Compound(BTreeMap<SNBTString, Expression>),
    DataTarget(DataTarget, Option<NbtPath>),
}

impl FunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleFunctionCommandArguments> {
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

                MiddleFunctionCommandArguments::Compound(compound)
            }
            Self::DataTarget(target, path) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;

                MiddleFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
