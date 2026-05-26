use minecraft_command_types::{command::execute::Rotated, rotation::Rotation};

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::entity_selector::SemanticEntitySelector,
};

#[derive(Debug, Clone)]
pub enum SemanticRotated {
    Rotation(Rotation),
    As(SemanticEntitySelector),
}

impl SemanticRotated {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Rotated {
        match self {
            Self::Rotation(rotation) => Rotated::Rotation(rotation),
            Self::As(selector) => Rotated::As(selector.compile(datapack, ctx)),
        }
    }
}
