use minecraft_command_types::coordinate::Coordinates;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext},
    low::item_source::ItemSource as MiddleItemSource,
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItemSource> {
        Some(match self {
            Self::Block(coordinates) => MiddleItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleItemSource::Entity(selector)
            }
        })
    }
}
