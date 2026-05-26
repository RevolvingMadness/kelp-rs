use minecraft_command_types::{command::execute::Rotated as LowRotated, rotation::Rotation};

use crate::{
    compile_context::CompileContext, datapack::Datapack, semantic::entity_selector::SemanticEntitySelector,
};

#[derive(Debug, Clone)]
pub enum Rotated {
    Rotation(Rotation),
    As(SemanticEntitySelector),
}

impl Rotated {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowRotated {
        match self {
            Self::Rotation(rotation) => LowRotated::Rotation(rotation),
            Self::As(selector) => LowRotated::As(selector.compile(datapack, ctx)),
        }
    }
}
