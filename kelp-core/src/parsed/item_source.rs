use minecraft_command_types::coordinate::Coordinates;

use crate::{
    parsed::{
        arena::ParsedAstArena, entity_selector::EntitySelector,
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::{arena::TypedAstArena, item_source::TypedItemSource},
};

#[derive(Debug, Clone)]
pub enum ItemSource {
    Block(Coordinates),
    Entity(EntitySelector),
}

impl ItemSource {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedItemSource> {
        Some(match self {
            Self::Block(coordinates) => TypedItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedItemSource::Entity(selector)
            }
        })
    }
}
