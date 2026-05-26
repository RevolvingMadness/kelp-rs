use minecraft_command_types::rotation::Rotation;

use crate::{
    parsed::{entity_selector::ParsedEntitySelector, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::execute::rotated::SemanticRotated,
};

#[derive(Debug, Clone)]
pub enum ParsedRotated {
    Rotation(Rotation),
    As(ParsedEntitySelector),
}

impl ParsedRotated {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticRotated> {
        Some(match self {
            Self::Rotation(rotation) => SemanticRotated::Rotation(rotation),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                SemanticRotated::As(selector)
            }
        })
    }
}
