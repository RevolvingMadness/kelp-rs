use minecraft_command_types::{command::execute::Rotated, rotation::Rotation};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighRotated {
    Rotation(Rotation),
    As(HighEntitySelector),
}

impl HighRotated {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Rotated {
        match self {
            Self::Rotation(rotation) => Rotated::Rotation(rotation),
            Self::As(selector) => Rotated::As(selector.compile(datapack, ctx)),
        }
    }
}
