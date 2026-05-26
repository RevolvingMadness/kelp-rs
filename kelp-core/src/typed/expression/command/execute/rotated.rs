use minecraft_command_types::{command::execute::Rotated, rotation::Rotation};

use crate::{
    compile_context::CompileContext, datapack::Datapack, typed::arena::TypedAstArena,
    typed::entity_selector::TypedEntitySelector,
};

#[derive(Debug, Clone)]
pub enum TypedRotated {
    Rotation(Rotation),
    As(TypedEntitySelector),
}

impl TypedRotated {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Rotated {
        match self {
            Self::Rotation(rotation) => Rotated::Rotation(rotation),
            Self::As(selector) => Rotated::As(selector.compile(arena, datapack, ctx)),
        }
    }
}
