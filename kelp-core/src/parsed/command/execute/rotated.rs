use minecraft_command_types::rotation::Rotation;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    typed::arena::TypedAstArena,
    typed::expression::command::execute::rotated::TypedRotated,
};

#[derive(Debug, Clone)]
pub enum ParsedRotated {
    Rotation(Rotation),
    As(EntitySelector),
}

impl ParsedRotated {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedRotated> {
        Some(match self {
            Self::Rotation(rotation) => TypedRotated::Rotation(rotation),
            Self::As(selector) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedRotated::As(selector)
            }
        })
    }
}
