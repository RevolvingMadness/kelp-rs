use minecraft_command_types::{
    command::item_source::ItemSource as LowItemSource, coordinate::Coordinates,
};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    low::entity_selector::EntitySelector,
};

#[derive(Debug, Clone)]
pub enum ItemSource {
    Block(Coordinates),
    Entity(EntitySelector),
}

impl ItemSource {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowItemSource {
        match self {
            Self::Block(coordinates) => LowItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.compile(allocator, datapack, ctx);

                LowItemSource::Entity(selector)
            }
        }
    }
}
