use std::fmt::{Display, Write};

use minecraft_command_types::entity_selector::EntitySelectorVariable;

use crate::{
    parsed::{
        entity_selector::option::ParsedEntitySelectorOption,
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::entity_selector::SemanticEntitySelector,
    trait_ext::CollectOptionAllIterExt,
};

pub mod option;

#[derive(Debug, Clone)]
pub enum ParsedEntitySelector {
    Variable(EntitySelectorVariable, Vec<ParsedEntitySelectorOption>),
    Name(String),
}

impl Display for ParsedEntitySelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(variable, options) => {
                f.write_char('@')?;
                variable.fmt(f)?;

                if !options.is_empty() {
                    f.write_str("[...]")?;
                }

                Ok(())
            }
            Self::Name(name) => f.write_str(name),
        }
    }
}

impl ParsedEntitySelector {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticEntitySelector> {
        Some(match self {
            Self::Variable(variable, options) => {
                let options = options
                    .into_iter()
                    .map(|option| option.perform_semantic_analysis(ctx))
                    .collect_option_all()?;

                SemanticEntitySelector::Variable(variable, options)
            }
            Self::Name(name) => SemanticEntitySelector::Name(name),
        })
    }
}
