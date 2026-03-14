use minecraft_command_types::command::execute::{
    ScoreComparison as LowScoreComparison, ScoreComparisonOperator,
};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types_derive::HasMacro;

use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::high::player_score::PlayerScore;
use crate::semantic_analysis_context::SemanticAnalysisContext;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
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

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowScoreComparison {
        match self {
            Self::Range(range) => LowScoreComparison::Range(range),
            Self::Score(operator, player_score) => {
                LowScoreComparison::Score(operator, player_score.compile(datapack, ctx).score)
            }
        }
    }
}
