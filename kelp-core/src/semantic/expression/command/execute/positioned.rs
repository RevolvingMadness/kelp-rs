use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned as LowPositioned},
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
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowPositioned {
        match self {
            Self::Position(position) => LowPositioned::Position(position),
            Self::As(selector) => LowPositioned::As(selector.compile(datapack, ctx)),
            Self::Over(heightmap) => LowPositioned::Over(heightmap),
        }
    }
}
