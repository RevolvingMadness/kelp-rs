use minecraft_command_types::{
    command::item_source::ItemSource as LowItemSource, coordinate::Coordinates,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack, middle::entity_selector::EntitySelector,
};

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ItemSource {
    Block(Coordinates),
    Entity(EntitySelector),
}

impl ItemSource {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemSource {
        match self {
            Self::Block(coordinates) => LowItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.compile(datapack, ctx);

                LowItemSource::Entity(selector)
            }
        }
    }
}
