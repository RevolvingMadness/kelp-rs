use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};

use crate::{
    parsed::{entity_selector::ParsedEntitySelector, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::execute::positioned::SemanticPositioned,
};

#[derive(Debug, Clone)]
pub enum Positioned {
    Position(Coordinates),
    As(ParsedEntitySelector),
    Over(Heightmap),
}

impl Positioned {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticPositioned> {
        Some(match self {
            Self::Position(coordinates) => SemanticPositioned::Position(coordinates),
            Self::As(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                SemanticPositioned::As(selector)
            }
            Self::Over(heightmap) => SemanticPositioned::Over(heightmap),
        })
    }
}
