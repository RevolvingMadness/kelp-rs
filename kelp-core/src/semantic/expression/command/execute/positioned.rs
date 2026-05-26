use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned},
    coordinate::Coordinates,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::entity_selector::SemanticEntitySelector,
};

#[derive(Debug, Clone)]
pub enum SemanticPositioned {
    Position(Coordinates),
    As(SemanticEntitySelector),
    Over(Heightmap),
}

impl SemanticPositioned {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Positioned {
        match self {
            Self::Position(position) => Positioned::Position(position),
            Self::As(selector) => Positioned::As(selector.compile(datapack, ctx)),
            Self::Over(heightmap) => Positioned::Over(heightmap),
        }
    }
}
