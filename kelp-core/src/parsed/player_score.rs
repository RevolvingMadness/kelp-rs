use std::fmt::{Display, Write};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        entity_selector::EntitySelector, semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    typed::arena::TypedAstArena,
    typed::player_score::TypedPlayerScore,
};

#[derive(Debug, Clone)]
pub struct PlayerScore {
    pub is_generated: bool,
    pub selector: Box<ParsedSupportsExpressionSigil<EntitySelector>>,
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedPlayerScore> {
        let selector = self
            .selector
            .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

        Some(TypedPlayerScore {
            is_generated: self.is_generated,
            selector: Box::new(selector),
            objective: self.objective,
        })
    }
}
