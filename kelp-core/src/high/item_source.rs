use minecraft_command_types::coordinate::Coordinates;

use crate::{
    high::{entity_selector::EntitySelector, semantic_analysis_context::SemanticAnalysisContext},
    middle::item_source::ItemSource as MiddleItemSource,
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItemSource> {
        Some(match self {
            Self::Block(coordinates) => MiddleItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddleItemSource::Entity(selector)
            }
        })
    }
}
