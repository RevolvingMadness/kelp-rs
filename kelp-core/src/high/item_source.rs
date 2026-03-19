use minecraft_command_types::coordinate::Coordinates;

use crate::{
    high::entity_selector::EntitySelector, middle::item_source::ItemSource as MiddleItemSource,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
