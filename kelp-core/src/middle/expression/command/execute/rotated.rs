use minecraft_command_types::{command::execute::Rotated as LowRotated, rotation::Rotation};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack, middle::entity_selector::EntitySelector,
};

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum Rotated {
    Rotation(Rotation),
    As(EntitySelector),
}

impl Rotated {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowRotated {
        match self {
            Self::Rotation(rotation) => LowRotated::Rotation(rotation),
            Self::As(selector) => LowRotated::As(selector.compile(datapack, ctx)),
        }
    }
}
