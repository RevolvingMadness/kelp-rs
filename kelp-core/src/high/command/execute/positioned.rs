use minecraft_command_types::{
    command::{enums::heightmap::Heightmap, execute::Positioned},
    coordinate::Coordinates,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighPositioned {
    Position(Coordinates),
    As(HighEntitySelector),
    Over(Heightmap),
}

impl HighPositioned {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Positioned {
        match self {
            HighPositioned::Position(position) => Positioned::Position(position),
            HighPositioned::As(selector) => Positioned::As(selector.compile(datapack, ctx)),
            HighPositioned::Over(heightmap) => Positioned::Over(heightmap),
        }
    }
}
