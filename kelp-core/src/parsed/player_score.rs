use std::fmt::{Display, Write};

use crate::{
    parsed::{
        entity_selector::ParsedEntitySelector, semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::player_score::SemanticPlayerScore as MiddlePlayerScore,
};

#[derive(Debug, Clone)]
pub struct PlayerScore {
    pub is_generated: bool,
    pub selector: Box<ParsedSupportsExpressionSigil<ParsedEntitySelector>>,
    pub objective: String,
}

impl Display for PlayerScore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("score ")?;

        self.selector.fmt(f)?;

        f.write_char(' ')?;

        f.write_str(&self.objective)?;

        Ok(())
    }
}

impl PlayerScore {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayerScore> {
        let selector = self.selector.perform_semantic_analysis(ctx)?;

        Some(MiddlePlayerScore {
            is_generated: self.is_generated,
            selector: Box::new(selector),
            objective: self.objective,
        })
    }
}
