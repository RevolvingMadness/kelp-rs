use minecraft_command_types::command::item_source::ItemSource;

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
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ItemSource {
        match self {
            Self::Block(coordinates) => {
                let coordinates = coordinates.compile(datapack, ctx);

                ItemSource::Block(coordinates)
            }
            Self::Entity(selector) => {
                let selector = selector.compile(datapack, ctx);

                ItemSource::Entity(selector)
            }
        }
    }
}
