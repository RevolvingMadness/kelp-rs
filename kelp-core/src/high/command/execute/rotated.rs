use minecraft_command_types::rotation::Rotation;
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::entity_selector::EntitySelector,
    middle::expression::command::execute::rotated::Rotated as MiddleRotated,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum Rotated {
    Rotation(Rotation),
    As(EntitySelector),
}

impl Rotated {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleRotated> {
        Some(match self {
            Self::Rotation(rotation) => MiddleRotated::Rotation(rotation),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleRotated::As(selector)
            }
        })
    }
}
