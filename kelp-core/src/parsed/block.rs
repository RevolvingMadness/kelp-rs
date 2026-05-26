use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::block::TypedBlockState as MiddleBlockState,
};

#[derive(Debug, Clone)]
pub struct BlockState {
    pub id: ResourceLocation,
    pub block_states: HashMap<String, String>,
    pub data_tags: Option<HashMap<SNBTString, ParsedExpressionId>>,
}

impl BlockState {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleBlockState> {
        let data_tags = match self.data_tags {
            Some(data_tags) => Some(
                data_tags
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, key) = key.perform_semantic_analysis(ctx);
                        let value = ParsedExpression::perform_semantic_analysis(
                            value,
                            parsed_arena,
                            typed_arena,
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
