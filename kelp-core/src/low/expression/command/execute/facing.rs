use minecraft_command_types::{
    command::{enums::entity_anchor::EntityAnchor, execute::Facing as LowFacing},
    coordinate::Coordinates,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, low::entity_selector::EntitySelector,
};

#[derive(Debug, Clone)]
pub enum Facing {
    Position(Coordinates),
    Entity(EntitySelector, EntityAnchor),
}

impl Facing {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowFacing {
        match self {
            Self::Position(position) => LowFacing::Position(position),
            Self::Entity(selector, anchor) => {
                LowFacing::Entity(selector.compile(datapack, ctx), anchor)
            }
        }
    }
}
