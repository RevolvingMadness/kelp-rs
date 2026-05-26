use minecraft_command_types::command::item_source::ItemSource as LowItemSource;

use crate::semantic::coordinate::SemanticCoordinates;
use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::entity_selector::SemanticEntitySelector,
};

#[derive(Debug, Clone)]
pub enum SemanticItemSource {
    Block(Box<SemanticCoordinates>),
    Entity(SemanticEntitySelector),
}

impl SemanticItemSource {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowItemSource {
        match self {
            Self::Block(coordinates) => {
                let coordinates = coordinates.compile(datapack, ctx);

                LowItemSource::Block(coordinates)
            }
            Self::Entity(selector) => {
                let selector = selector.compile(datapack, ctx);

                LowItemSource::Entity(selector)
            }
        }
    }
}
