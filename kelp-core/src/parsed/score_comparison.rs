use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::range::IntegerRange;

use crate::parsed::arena::ParsedAstArena;
use crate::parsed::player_score::PlayerScore;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::typed::arena::TypedAstArena;
use crate::typed::score_comparison::TypedScoreComparison as MiddleScoreComparison;

#[derive(Debug, Clone)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreComparison> {
        Some(match self {
            Self::Range(range) => MiddleScoreComparison::Range(range),
            Self::Score(operator, score) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                MiddleScoreComparison::Score(operator, score)
            }
        })
    }
}
