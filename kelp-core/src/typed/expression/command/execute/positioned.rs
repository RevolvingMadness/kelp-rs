use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned},
    coordinate::Coordinates,
};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    typed::entity_selector::TypedEntitySelector,
};

#[derive(Debug, Clone)]
pub enum TypedPositioned {
    Position(Coordinates),
    As(TypedEntitySelector),
    Over(Heightmap),
}

impl TypedPositioned {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Positioned {
        match self {
            Self::Position(position) => Positioned::Position(position),
            Self::As(selector) => Positioned::As(selector.compile(allocator, datapack, ctx)),
            Self::Over(heightmap) => Positioned::Over(heightmap),
        }
    }
}
