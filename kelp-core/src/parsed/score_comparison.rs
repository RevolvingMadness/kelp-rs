use minecraft_command_types::command::execute::ScoreComparisonOperator;
use minecraft_command_types::range::IntegerRange;

use crate::parsed::player_score::PlayerScore;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::semantic::score_comparison::SemanticScoreComparison;

#[derive(Debug, Clone)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticScoreComparison> {
        Some(match self {
            Self::Range(range) => SemanticScoreComparison::Range(range),
            Self::Score(operator, score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticScoreComparison::Score(operator, score)
            }
        })
    }
}
