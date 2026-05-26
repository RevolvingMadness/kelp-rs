use std::fmt::{Display, Write};

use minecraft_command_types::entity_selector::EntitySelectorVariable;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        entity_selector::option::ParsedEntitySelectorOption,
        semantic_analysis::SemanticAnalysisContext,
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::entity_selector::TypedEntitySelector,
};

pub mod option;

#[derive(Debug, Clone)]
pub enum EntitySelector {
    Variable(EntitySelectorVariable, Vec<ParsedEntitySelectorOption>),
    Name(String),
}

impl Display for EntitySelector {
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

impl EntitySelector {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedEntitySelector> {
        Some(match self {
            Self::Variable(variable, options) => {
                let options = options
                    .into_iter()
                    .map(|option| option.perform_semantic_analysis(parsed_arena, typed_arena, ctx))
                    .collect_option_all()?;

                TypedEntitySelector::Variable(variable, options)
            }
            Self::Name(name) => TypedEntitySelector::Name(name),
        })
    }
}
