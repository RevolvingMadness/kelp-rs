use minecraft_command_types::{command::enums::heightmap::Heightmap, coordinate::Coordinates};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    typed::arena::TypedAstArena,
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedPositioned> {
        Some(match self {
            Self::Position(coordinates) => TypedPositioned::Position(coordinates),
            Self::As(selector) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPositioned::As(selector)
            }
            Self::Over(heightmap) => TypedPositioned::Over(heightmap),
        })
    }
}
