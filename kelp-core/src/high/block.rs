use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        expression::{Expression, ExpressionId},
        semantic_analysis::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    low::block::BlockState as MiddleBlockState,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub struct BlockState {
    pub id: ResourceLocation,
    pub block_states: HashMap<String, String>,
    pub data_tags: Option<HashMap<SNBTString, ExpressionId>>,
}

impl BlockState {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleBlockState> {
        let data_tags = match self.data_tags {
            Some(data_tags) => Some(
                data_tags
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let value = Expression::perform_semantic_analysis(
                            value,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

                        Some((key, value))
                    })
                    .collect_option_all()?,
            ),
            None => None,
        };

        Some(MiddleBlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags,
        })
    }
}
