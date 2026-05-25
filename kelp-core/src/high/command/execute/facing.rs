use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    low::expression::command::execute::facing::Facing as MiddleFacing,
};

#[derive(Debug, Clone)]
pub enum Facing {
    Position(Coordinates),
    Entity(EntitySelector, EntityAnchor),
}

impl Facing {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleFacing> {
        Some(match self {
            Self::Position(coordinates) => MiddleFacing::Position(coordinates),
            Self::Entity(selector, anchor) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleFacing::Entity(selector, anchor)
            }
        })
    }
}
