use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::range::IntegerRange;

use crate::ast_allocator::high::HighAstAllocator;
use crate::ast_allocator::low::LowAstAllocator;
use crate::high::player_score::PlayerScore;
use crate::high::semantic_analysis::SemanticAnalysisContext;
use crate::low::score_comparison::ScoreComparison as MiddleScoreComparison;

#[derive(Debug, Clone)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreComparison> {
        Some(match self {
            Self::Range(range) => MiddleScoreComparison::Range(range),
            Self::Score(operator, score) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleScoreComparison::Score(operator, score)
            }
        })
    }
}
