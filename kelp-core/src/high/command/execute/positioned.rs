use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::entity_selector::EntitySelector,
    middle::expression::command::execute::positioned::Positioned as MiddlePositioned,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum Positioned {
    Position(Coordinates),
    As(EntitySelector),
    Over(Heightmap),
}

impl Positioned {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddlePositioned> {
        Some(match self {
            Self::Position(coordinates) => MiddlePositioned::Position(coordinates),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePositioned::As(selector)
            }
            Self::Over(heightmap) => MiddlePositioned::Over(heightmap),
        })
    }
}
