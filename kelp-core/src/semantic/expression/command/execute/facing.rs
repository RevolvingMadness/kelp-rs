use minecraft_command_types::{
    command::{enums::entity_anchor::EntityAnchor, execute::Facing},
    coordinate::Coordinates,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::entity_selector::SemanticEntitySelector,
};

#[derive(Debug, Clone)]
pub enum SemanticFacing {
    Position(Coordinates),
    Entity(SemanticEntitySelector, EntityAnchor),
}

impl SemanticFacing {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Facing {
        match self {
            Self::Position(position) => Facing::Position(position),
            Self::Entity(selector, anchor) => {
                Facing::Entity(selector.compile(datapack, ctx), anchor)
            }
        }
    }
}
