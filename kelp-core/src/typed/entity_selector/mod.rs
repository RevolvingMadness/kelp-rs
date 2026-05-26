use minecraft_command_types::entity_selector::{EntitySelector, EntitySelectorVariable};

use crate::{
    ast_allocator::low::LowAstAllocator, compile_context::CompileContext, datapack::Datapack,
    typed::entity_selector::option::TypedEntitySelectorOption,
};

pub mod option;

#[derive(Debug, Clone)]
pub enum TypedEntitySelector {
    Variable(EntitySelectorVariable, Vec<TypedEntitySelectorOption>),
    Name(String),
}

impl TypedEntitySelector {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> EntitySelector {
        match self {
            Self::Variable(variable, options) => EntitySelector::Variable(
                variable,
                options
                    .into_iter()
                    .map(|option| option.compile(allocator, datapack, ctx))
                    .collect(),
            ),
            Self::Name(name) => EntitySelector::Name(name),
        }
    }

    pub fn compile_as_statement(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        match self {
            Self::Variable(_, options) => {
                for option in options {
                    option.compile_as_statement(allocator, datapack, ctx);
                }
            }
            Self::Name(..) => {}
        }
    }
}
