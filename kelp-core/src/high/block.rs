use std::collections::BTreeMap;

use minecraft_command_types::{block::BlockState, resource_location::ResourceLocation};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack, expression::ExpressionCompoundKind,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighBlockState {
    pub id: ResourceLocation,
    pub block_states: BTreeMap<String, String>,
    pub data_tags: Option<ExpressionCompoundKind>,
}

impl HighBlockState {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        if let Some(data_tags) = &self.data_tags {
            data_tags
                .values()
                .map(|data_tag| data_tag.perform_semantic_analysis(ctx, is_lhs, None))
                .all_some()
        } else {
            Some(())
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> BlockState {
        BlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags: self.data_tags.map(|value| {
                value
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key.snbt_string, value)
                    })
                    .collect()
            }),
        }
    }
}
