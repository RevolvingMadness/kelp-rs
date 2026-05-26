use minecraft_command_types::entity_selector::{EntitySelector, EntitySelectorVariable};

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    semantic::entity_selector::option::SemanticEntitySelectorOption,
};

pub mod option;

#[derive(Debug, Clone)]
pub enum SemanticEntitySelector {
    Variable(EntitySelectorVariable, Vec<SemanticEntitySelectorOption>),
    Name(String),
}

impl SemanticEntitySelector {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> EntitySelector {
        match self {
            Self::Variable(variable, options) => EntitySelector::Variable(
                variable,
                options
                    .into_iter()
                    .map(|option| option.compile(datapack, ctx))
                    .collect(),
            ),
            Self::Name(name) => EntitySelector::Name(name),
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Variable(_, options) => {
                for option in options {
                    option.compile_as_statement(datapack, ctx);
                }
            }
            Self::Name(..) => {}
        }
    }
}
