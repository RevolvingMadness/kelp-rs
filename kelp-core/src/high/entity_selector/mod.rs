use minecraft_command_types::entity_selector::{EntitySelector, EntitySelectorVariable};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::option::HighEntitySelectorOption,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::OptionUnitIterExt,
};

pub mod option;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighEntitySelector {
    Variable(EntitySelectorVariable, Vec<HighEntitySelectorOption>),
    Name(String),
}

impl HighEntitySelector {
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

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> EntitySelector {
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
}
