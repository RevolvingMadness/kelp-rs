use minecraft_command_types::rotation::Rotation;

use crate::{
    parsed::{entity_selector::ParsedEntitySelector, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::execute::rotated::Rotated as MiddleRotated,
};

#[derive(Debug, Clone)]
pub enum Rotated {
    Rotation(Rotation),
    As(ParsedEntitySelector),
}

impl Rotated {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleRotated> {
        Some(match self {
            Self::Rotation(rotation) => MiddleRotated::Rotation(rotation),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddleRotated::As(selector)
            }
        })
    }
}
