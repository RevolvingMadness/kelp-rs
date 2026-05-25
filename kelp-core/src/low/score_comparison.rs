use minecraft_command_types::command::execute::{
    ScoreComparison as LowScoreComparison, ScoreComparisonOperator,
};
use minecraft_command_types::range::IntegerRange;

use crate::ast_allocator::low::LowAstAllocator;
use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::low::player_score::PlayerScore;

#[derive(Debug, Clone)]
pub enum ScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, PlayerScore),
}

impl ScoreComparison {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreComparison {
        match self {
            Self::Range(range) => LowScoreComparison::Range(range),
            Self::Score(operator, player_score) => LowScoreComparison::Score(
                operator,
                player_score.compile(allocator, datapack, ctx).score,
            ),
        }
    }
}
