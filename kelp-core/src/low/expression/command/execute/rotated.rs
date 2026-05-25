use minecraft_command_types::{command::execute::Rotated as LowRotated, rotation::Rotation};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    low::entity_selector::EntitySelector,
};

#[derive(Debug, Clone)]
pub enum Rotated {
    Rotation(Rotation),
    As(EntitySelector),
}

impl Rotated {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowRotated {
        match self {
            Self::Rotation(rotation) => LowRotated::Rotation(rotation),
            Self::As(selector) => LowRotated::As(selector.compile(allocator, datapack, ctx)),
        }
    }
}
