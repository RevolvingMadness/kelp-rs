use minecraft_command_types::{
    command::{enums::entity_anchor::EntityAnchor, execute::Facing},
    coordinate::Coordinates,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
    typed::entity_selector::TypedEntitySelector,
};

#[derive(Debug, Clone)]
pub enum TypedFacing {
    Position(Coordinates),
    Entity(TypedEntitySelector, EntityAnchor),
}

impl TypedFacing {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Facing {
        match self {
            Self::Position(position) => Facing::Position(position),
            Self::Entity(selector, anchor) => {
                Facing::Entity(selector.compile(arena, datapack, ctx), anchor)
            }
        }
    }
}
