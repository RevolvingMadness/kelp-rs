use std::collections::BTreeMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        expression::Expression, semantic_analysis_context::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    middle::block::BlockState as MiddleBlockState,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockState {
    pub id: ResourceLocation,
    pub block_states: BTreeMap<String, String>,
    pub data_tags: Option<BTreeMap<SNBTString, Expression>>,
}

impl BlockState {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleBlockState> {
        let data_tags = match self.data_tags {
            Some(data_tags) => Some(
                data_tags
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key, value))
                    })
                    .collect_option_all()?,
            ),
            None => None,
        };

        Some(MiddleBlockState {
            id: self.id,
            block_states: self.block_states,
            data_tags,
        })
    }
}
