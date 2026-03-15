use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::entity_selector::EntitySelector,
    middle::expression::command::execute::facing::Facing as MiddleFacing,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum Facing {
    Position(Coordinates),
    Entity(EntitySelector, EntityAnchor),
}

impl Facing {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleFacing> {
        Some(match self {
            Self::Position(coordinates) => MiddleFacing::Position(coordinates),
            Self::Entity(selector, anchor) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleFacing::Entity(selector, anchor)
            }
        })
    }
}
