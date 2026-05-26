use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned},
    coordinate::Coordinates,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
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
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Positioned {
        match self {
            Self::Position(position) => Positioned::Position(position),
            Self::As(selector) => Positioned::As(selector.compile(arena, datapack, ctx)),
            Self::Over(heightmap) => Positioned::Over(heightmap),
        }
    }
}
