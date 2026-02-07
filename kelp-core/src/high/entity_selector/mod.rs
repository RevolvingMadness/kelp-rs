use minecraft_command_types::entity_selector::{EntitySelector, EntitySelectorVariable};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::option::HighEntitySelectorOption,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionIterExt,
};

pub mod option;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighEntitySelector {
    Variable(EntitySelectorVariable, Vec<HighEntitySelectorOption>),
    Name(String),
}

impl HighEntitySelector {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match self {
            HighEntitySelector::Variable(_, options) => options
                .iter()
                .map(|option| option.perform_semantic_analysis(ctx))
                .all_some(),
            HighEntitySelector::Name(_) => Some(()),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> EntitySelector {
        match self {
            HighEntitySelector::Variable(variable, options) => EntitySelector::Variable(
                variable,
                options
                    .into_iter()
                    .map(|option| option.compile(datapack, ctx))
                    .collect(),
            ),
            HighEntitySelector::Name(name) => EntitySelector::Name(name),
        }
    }
}
