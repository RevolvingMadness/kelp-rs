use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::range::IntegerRange;

use crate::parsed::player_score::PlayerScore;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::semantic::score_comparison::ScoreComparison as MiddleScoreComparison;

#[derive(Debug, Clone)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreComparison> {
        Some(match self {
            Self::Range(range) => MiddleScoreComparison::Range(range),
            Self::Score(operator, score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddleScoreComparison::Score(operator, score)
            }
        })
    }
}
