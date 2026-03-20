use crate::{
    high::{entity_selector::EntitySelector, semantic_analysis_context::SemanticAnalysisContext},
    middle::player_score::PlayerScore as MiddlePlayerScore,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct PlayerScore {
    pub is_generated: bool,
    pub selector: EntitySelector,
    pub objective: String,
}

impl PlayerScore {
    #[must_use]
    pub const fn new(selector: EntitySelector, objective: String) -> Self {
        Self {
            is_generated: false,
            selector,
            objective,
        }
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayerScore> {
        let selector = self.selector.perform_semantic_analysis(ctx)?;

        Some(MiddlePlayerScore {
            is_generated: self.is_generated,
            selector,
            objective: self.objective,
        })
    }
}
