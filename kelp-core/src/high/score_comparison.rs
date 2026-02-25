use minecraft_command_types::command::execute::{ScoreComparison, ScoreComparisonOperator};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types_derive::HasMacro;

use crate::compile_context::CompileContext;
use crate::datapack::HighDatapack;
use crate::high::player_score::HighPlayerScore;
use crate::semantic_analysis_context::SemanticAnalysisContext;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, HighPlayerScore),
}

impl HighScoreComparison {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Range(_) => Some(()),
            Self::Score(_, score) => score.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> ScoreComparison {
        match self {
            Self::Range(range) => ScoreComparison::Range(range),
            Self::Score(operator, player_score) => {
                ScoreComparison::Score(operator, player_score.compile(datapack, ctx).score)
            }
        }
    }
}
