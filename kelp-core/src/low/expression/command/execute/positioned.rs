use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned as LowPositioned},
    coordinate::Coordinates,
};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    low::entity_selector::EntitySelector,
};

#[derive(Debug, Clone)]
pub enum Positioned {
    Position(Coordinates),
    As(EntitySelector),
    Over(Heightmap),
}

impl Positioned {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowPositioned {
        match self {
            Self::Position(position) => LowPositioned::Position(position),
            Self::As(selector) => LowPositioned::As(selector.compile(allocator, datapack, ctx)),
            Self::Over(heightmap) => LowPositioned::Over(heightmap),
        }
    }
}
