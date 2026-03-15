use minecraft_command_types::coordinate::Coordinates;
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::entity_selector::EntitySelector, middle::item_source::ItemSource as MiddleItemSource,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ItemSource {
    Block(Coordinates),
    Entity(EntitySelector),
}

impl ItemSource {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleItemSource> {
        Some(match self {
            Self::Block(coordinates) => MiddleItemSource::Block(coordinates),
            Self::Entity(selector) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleItemSource::Entity(selector)
            }
        })
    }
}
