use crate::{
    parsed::{
        coordinate::ParsedCoordinates, entity_selector::ParsedEntitySelector,
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::item_source::SemanticItemSource as MiddleItemSource,
};

#[derive(Debug, Clone)]
pub enum ParsedItemSource {
    Block(Box<ParsedCoordinates>),
    Entity(ParsedEntitySelector),
}

impl ParsedItemSource {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItemSource> {
        Some(match self {
            Self::Block(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                MiddleItemSource::Block(Box::new(coordinates))
            }
            Self::Entity(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddleItemSource::Entity(selector)
            }
        })
    }
}
