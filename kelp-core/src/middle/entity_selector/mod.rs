use minecraft_command_types::entity_selector::{
    EntitySelector as LowEntitySelector, EntitySelectorVariable,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    middle::entity_selector::option::EntitySelectorOption,
};

pub mod option;

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum EntitySelector {
    Variable(EntitySelectorVariable, Vec<EntitySelectorOption>),
    Name(String),
}

impl EntitySelector {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowEntitySelector {
        match self {
            Self::Variable(variable, options) => LowEntitySelector::Variable(
                variable,
                options
                    .into_iter()
                    .map(|option| option.compile(datapack, ctx))
                    .collect(),
            ),
            Self::Name(name) => LowEntitySelector::Name(name),
        }
    }
}
