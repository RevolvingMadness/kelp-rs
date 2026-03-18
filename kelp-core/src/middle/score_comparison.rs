use minecraft_command_types::command::execute::{
    ScoreComparison as LowScoreComparison, ScoreComparisonOperator,
};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types_derive::HasMacro;

use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::middle::player_score::PlayerScore;

#[derive(Debug, Clone, HasMacro)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowScoreComparison {
        match self {
            Self::Range(range) => LowScoreComparison::Range(range),
            Self::Score(operator, player_score) => {
                LowScoreComparison::Score(operator, player_score.compile(datapack, ctx).score)
            }
        }
    }
}
