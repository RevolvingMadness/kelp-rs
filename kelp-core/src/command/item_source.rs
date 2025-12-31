use minecraft_command_types::{command::item_source::ItemSource, coordinate::Coordinates};
use minecraft_command_types_derive::HasMacro;

use crate::{
    command::context::CompileContext, datapack::HighDatapack, entity_selector::HighEntitySelector,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighItemSource {
    Block(Coordinates),
    Entity(HighEntitySelector),
}

impl HighItemSource {
    pub fn compile(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ItemSource {
        match self {
            HighItemSource::Block(coordinates) => ItemSource::Block(*coordinates),
            HighItemSource::Entity(selector) => {
                let selector = selector.clone().compile(datapack, ctx);

                ItemSource::Entity(selector)
            }
        }
    }
}
