use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};

use crate::{
    parsed::{entity_selector::ParsedEntitySelector, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::execute::facing::SemanticFacing as MiddleFacing,
};

#[derive(Debug, Clone)]
pub enum Facing {
    Position(Coordinates),
    Entity(ParsedEntitySelector, EntityAnchor),
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
