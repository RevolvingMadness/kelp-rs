use minecraft_command_types::entity_selector::{
    EntitySelector as LowEntitySelector, EntitySelectorVariable,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::Datapack,
    high::entity_selector::option::EntitySelectorOption,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionUnitIterExt,
};

pub mod option;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum EntitySelector {
    Variable(EntitySelectorVariable, Vec<EntitySelectorOption>),
    Name(String),
}

impl EntitySelector {
    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Variable(_, options) => options
                .iter()
                .map(|option| option.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            Self::Name(_) => Some(()),
        }
    }

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
