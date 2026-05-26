use minecraft_command_types::{
    command::enums::entity_anchor::EntityAnchor, coordinate::Coordinates,
};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    typed::arena::TypedAstArena,
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedFacing> {
        Some(match self {
            Self::Position(coordinates) => TypedFacing::Position(coordinates),
            Self::Entity(selector, anchor) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedFacing::Entity(selector, anchor)
            }
        })
    }
}
