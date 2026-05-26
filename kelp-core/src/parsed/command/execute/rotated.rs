use minecraft_command_types::rotation::Rotation;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedRotated> {
        Some(match self {
            Self::Rotation(rotation) => TypedRotated::Rotation(rotation),
            Self::As(selector) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedRotated::As(selector)
            }
        })
    }
}
