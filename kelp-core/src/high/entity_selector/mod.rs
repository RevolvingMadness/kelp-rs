use minecraft_command_types::entity_selector::EntitySelectorVariable;
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::entity_selector::option::EntitySelectorOption,
    middle::entity_selector::EntitySelector as MiddleEntitySelector,
    semantic_analysis_context::SemanticAnalysisContext, trait_ext::CollectOptionAllIterExt,
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
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleEntitySelector> {
        Some(match self {
            Self::Variable(variable, options) => {
                let options = options
                    .into_iter()
                    .map(|option| option.perform_semantic_analysis(ctx, is_lhs))
                    .collect_option_all()?;

                MiddleEntitySelector::Variable(variable, options)
            }
            Self::Name(name) => MiddleEntitySelector::Name(name),
        })
    }
}
