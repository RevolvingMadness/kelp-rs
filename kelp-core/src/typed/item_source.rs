use minecraft_command_types::{command::item_source::ItemSource, coordinate::Coordinates};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ItemSource {
        match self {
            Self::Block(coordinates) => ItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.compile(allocator, datapack, ctx);

                ItemSource::Entity(selector)
            }
        }
    }
}
