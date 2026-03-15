use std::collections::BTreeMap;

use minecraft_command_types_derive::HasMacro;

use crate::{
    high::{data::DataTarget, expression::Expression, nbt_path::NbtPath, snbt_string::SNBTString},
    middle::expression::command::function::FunctionCommandArguments as MiddleFunctionCommandArguments,
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum FunctionCommandArguments {
    Compound(BTreeMap<SNBTString, Expression>),
    DataTarget(DataTarget, Option<NbtPath>),
}

impl FunctionCommandArguments {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleFunctionCommandArguments> {
        Some(match self {
            Self::Compound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx, is_lhs);
                        let value = value.perform_semantic_analysis(ctx, is_lhs);

                        let (_, value) = value?;

                        Some((key, value))
                    })
                    .collect_option_all()?;

                MiddleFunctionCommandArguments::Compound(compound)
            }
            Self::DataTarget(target, path) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx, is_lhs)?),
                    None => None,
                };

                let target = target?;

                MiddleFunctionCommandArguments::DataTarget(target, path)
            }
        })
    }
}
