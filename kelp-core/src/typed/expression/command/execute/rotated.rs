use minecraft_command_types::{command::execute::Rotated, rotation::Rotation};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Rotated {
        match self {
            Self::Rotation(rotation) => Rotated::Rotation(rotation),
            Self::As(selector) => Rotated::As(selector.compile(allocator, datapack, ctx)),
        }
    }
}
