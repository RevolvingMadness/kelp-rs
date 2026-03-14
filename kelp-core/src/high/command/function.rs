use minecraft_command_types::command::function::FunctionCommandArguments as LowFunctionCommandArguments;
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::Datapack,
    high::expression::ExpressionCompoundKind,
    high::{data::DataTarget, nbt_path::NbtPath},
    semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum FunctionCommandArguments {
    Compound(ExpressionCompoundKind),
    DataTarget(DataTarget, Option<NbtPath>),
}

impl FunctionCommandArguments {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Compound(compound) => compound
                .values()
                .map(|value| {
                    value.perform_semantic_analysis(
                        ctx,
                        is_lhs,
                        Some(&DataTypeKind::Compound(Box::new(DataTypeKind::SNBT))),
                    )
                })
                .all_some(),
            Self::DataTarget(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path
                    .as_ref()
                    .map_or(Some(()), |path| path.perform_semantic_analysis(ctx, is_lhs));

                target_result?;
                path_result?;

                Some(())
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowFunctionCommandArguments {
        match self {
            Self::Compound(compound) => LowFunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect(),
            ),
            Self::DataTarget(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                LowFunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
