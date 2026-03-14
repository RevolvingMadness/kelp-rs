use std::collections::BTreeMap;

use minecraft_command_types::{
    block::BlockState as LowBlockState, resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    high::expression::ExpressionCompoundKind, semantic_analysis_context::SemanticAnalysisContext,
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct BlockState {
    pub id: ResourceLocation,
    pub block_states: BTreeMap<String, String>,
    pub data_tags: Option<ExpressionCompoundKind>,
}

impl BlockState {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        self.data_tags.as_ref().map_or(Some(()), |data_tags| {
            data_tags
                .values()
                .map(|data_tag| data_tag.perform_semantic_analysis(ctx, is_lhs, None))
                .all_some()
        })
    }

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowBlockState {
        LowBlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags: self.data_tags.map(|value| {
                value
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect()
            }),
        }
    }
}
