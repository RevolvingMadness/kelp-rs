use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    typed::expression::command::execute::facing::TypedFacing,
};

#[derive(Debug, Clone)]
pub enum ParsedFacing {
    Position(Coordinates),
    Entity(EntitySelector, EntityAnchor),
}

impl ParsedFacing {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedFacing> {
        Some(match self {
            Self::Position(coordinates) => TypedFacing::Position(coordinates),
            Self::Entity(selector, anchor) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedFacing::Entity(selector, anchor)
            }
        })
    }
}
