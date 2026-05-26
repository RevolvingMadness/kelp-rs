use minecraft_command_types::{command::item_source::ItemSource, coordinate::Coordinates};

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
    typed::entity_selector::TypedEntitySelector,
};

#[derive(Debug, Clone)]
pub enum TypedItemSource {
    Block(Coordinates),
    Entity(TypedEntitySelector),
}

impl TypedItemSource {
    #[must_use]
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ItemSource {
        match self {
            Self::Block(coordinates) => ItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.compile(arena, datapack, ctx);

                ItemSource::Entity(selector)
            }
        }
    }
}
