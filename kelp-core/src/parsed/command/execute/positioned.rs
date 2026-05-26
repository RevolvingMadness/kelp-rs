use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    typed::expression::command::execute::positioned::TypedPositioned,
};

#[derive(Debug, Clone)]
pub enum ParsedPositioned {
    Position(Coordinates),
    As(EntitySelector),
    Over(Heightmap),
}

impl ParsedPositioned {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedPositioned> {
        Some(match self {
            Self::Position(coordinates) => TypedPositioned::Position(coordinates),
            Self::As(selector) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedPositioned::As(selector)
            }
            Self::Over(heightmap) => TypedPositioned::Over(heightmap),
        })
    }
}
