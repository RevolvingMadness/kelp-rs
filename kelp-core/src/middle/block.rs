use std::collections::BTreeMap;

use minecraft_command_types::{
    block::BlockState as LowBlockState, resource_location::ResourceLocation, snbt::SNBTString,
};

use crate::{compile_context::CompileContext, datapack::Datapack, middle::expression::Expression};

#[derive(Debug, Clone)]
pub struct BlockState {
    pub id: ResourceLocation,
    pub block_states: BTreeMap<String, String>,
    pub data_tags: Option<BTreeMap<SNBTString, Expression>>,
}

impl BlockState {
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
