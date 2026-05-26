use std::collections::HashMap;

use minecraft_command_types::{
    block::BlockState as LowBlockState, resource_location::ResourceLocation, snbt::SNBTString,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, semantic::expression::SemanticExpression,
};

#[derive(Debug, Clone)]
pub struct SemanticBlockState {
    pub id: ResourceLocation,
    pub block_states: HashMap<String, String>,
    pub data_tags: Option<HashMap<SNBTString, SemanticExpression>>,
}

impl SemanticBlockState {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowBlockState {
        LowBlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags: self.data_tags.map(|value| {
                value
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect()
            }),
        }
    }
}
