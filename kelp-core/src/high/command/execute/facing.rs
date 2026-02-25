use minecraft_command_types::{
    command::{enums::entity_anchor::EntityAnchor, execute::Facing},
    coordinate::Coordinates,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighFacing {
    Position(Coordinates),
    Entity(HighEntitySelector, EntityAnchor),
}

impl HighFacing {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Facing {
        match self {
            Self::Position(position) => Facing::Position(position),
            Self::Entity(selector, anchor) => {
                Facing::Entity(selector.compile(datapack, ctx), anchor)
            }
        }
    }
}
