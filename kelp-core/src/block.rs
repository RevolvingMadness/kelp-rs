use std::collections::BTreeMap;

use minecraft_command_types::{block::BlockState, resource_location::ResourceLocation};
use minecraft_command_types_derive::HasMacro;

use crate::{
    command::context::CompileContext, datapack::HighDatapack, expression::ExpressionCompoundKind,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighBlockState {
    pub id: ResourceLocation,
    pub block_states: BTreeMap<String, String>,
    pub data_tags: Option<ExpressionCompoundKind>,
}

impl HighBlockState {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> BlockState {
        BlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags: self.data_tags.map(|value| {
                value
                    .into_iter()
                    .map(|(key, value)| {
                        let key = key.compile(datapack, ctx);
                        let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect()
            }),
        }
    }
}
