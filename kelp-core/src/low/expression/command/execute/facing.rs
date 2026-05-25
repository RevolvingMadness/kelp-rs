use minecraft_command_types::{
    command::{enums::entity_anchor::EntityAnchor, execute::Facing as LowFacing},
    coordinate::Coordinates,
};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    low::entity_selector::EntitySelector,
};

#[derive(Debug, Clone)]
pub enum Facing {
    Position(Coordinates),
    Entity(EntitySelector, EntityAnchor),
}

impl Facing {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowFacing {
        match self {
            Self::Position(position) => LowFacing::Position(position),
            Self::Entity(selector, anchor) => {
                LowFacing::Entity(selector.compile(allocator, datapack, ctx), anchor)
            }
        }
    }
}
