use minecraft_command_types::command::function::FunctionCommandArguments;
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::ExpressionCompoundKind,
    high::{data::HighDataTarget, nbt_path::HighNbtPath},
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighFunctionCommandArguments {
    Compound(ExpressionCompoundKind),
    DataTarget(HighDataTarget, Option<HighNbtPath>),
}

impl HighFunctionCommandArguments {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighFunctionCommandArguments::Compound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            HighFunctionCommandArguments::DataTarget(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path
                    .as_ref()
                    .map(|path| path.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                target_result?;
                path_result?;

                Some(())
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> FunctionCommandArguments {
        match self {
            HighFunctionCommandArguments::Compound(compound) => FunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect(),
            ),
            HighFunctionCommandArguments::DataTarget(target, path) => {
                let target = target.clone().compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                FunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
