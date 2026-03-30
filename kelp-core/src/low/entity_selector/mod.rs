use minecraft_command_types::entity_selector::{
    EntitySelector as LowEntitySelector, EntitySelectorVariable,
};

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::entity_selector::option::EntitySelectorOption,
};

pub mod option;

#[derive(Debug, Clone)]
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

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Variable(_, options) => {
                for option in options {
                    option.compile_as_statement(datapack, ctx);
                }
            }
            Self::Name(_) => {}
        }
    }
}
