use minecraft_command_types::command::execute::{ScoreComparison, ScoreComparisonOperator};
use minecraft_command_types::range::IntegerRange;

use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::semantic::player_score::SemanticPlayerScore;

#[derive(Debug, Clone)]
pub enum SemanticScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, SemanticPlayerScore),
}

impl SemanticScoreComparison {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ScoreComparison {
        match self {
            Self::Range(range) => ScoreComparison::Range(range),
            Self::Score(operator, player_score) => {
                ScoreComparison::Score(operator, player_score.compile(datapack, ctx).score)
            }
        }
    }
}
