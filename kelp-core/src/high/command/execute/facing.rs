use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};

use crate::{
    high::{entity_selector::EntitySelector, semantic_analysis_context::SemanticAnalysisContext},
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleFacing> {
        Some(match self {
            Self::Position(coordinates) => MiddleFacing::Position(coordinates),
            Self::Entity(selector, anchor) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddleFacing::Entity(selector, anchor)
            }
        })
    }
}
