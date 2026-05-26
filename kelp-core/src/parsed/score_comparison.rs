use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::range::IntegerRange;

use crate::parsed::arena::ParsedAstArena;
use crate::parsed::player_score::PlayerScore;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::typed::arena::TypedAstArena;
use crate::typed::score_comparison::TypedScoreComparison;

#[derive(Debug, Clone)]
pub enum ParsedScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ParsedScoreComparison {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedScoreComparison> {
        Some(match self {
            Self::Range(range) => TypedScoreComparison::Range(range),
            Self::Score(operator, score) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedScoreComparison::Score(operator, score)
            }
        })
    }
}
