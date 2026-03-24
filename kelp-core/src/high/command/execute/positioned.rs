use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};

use crate::{
    high::{entity_selector::EntitySelector, semantic_analysis_context::SemanticAnalysisContext},
    low::expression::command::execute::positioned::Positioned as MiddlePositioned,
};

#[derive(Debug, Clone)]
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
    ) -> Option<MiddlePositioned> {
        Some(match self {
            Self::Position(coordinates) => MiddlePositioned::Position(coordinates),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddlePositioned::As(selector)
            }
            Self::Over(heightmap) => MiddlePositioned::Over(heightmap),
        })
    }
}
