use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePositioned> {
        Some(match self {
            Self::Position(coordinates) => MiddlePositioned::Position(coordinates),
            Self::As(selector) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePositioned::As(selector)
            }
            Self::Over(heightmap) => MiddlePositioned::Over(heightmap),
        })
    }
}
