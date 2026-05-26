use std::collections::HashMap;

use minecraft_command_types::{
    block::BlockState, resource_location::ResourceLocation, snbt::SNBTString,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::expression::{TypedExpression, TypedExpressionId},
};

#[derive(Debug, Clone)]
pub struct TypedBlockState {
    pub id: ResourceLocation,
    pub block_states: HashMap<String, String>,
    pub data_tags: Option<HashMap<SNBTString, TypedExpressionId>>,
}

impl TypedBlockState {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> BlockState {
        BlockState {
            id: self.id,
            block_states: self.block_states.into_iter().collect(),
            data_tags: self.data_tags.map(|value| {
                value
                    .into_iter()
                    .map(|(key, value)| {
                        let value = TypedExpression::resolve(value, arena, datapack, ctx)
                            .as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect()
            }),
        }
    }
}
