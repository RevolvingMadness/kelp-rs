use minecraft_command_types::command::execute::{ScoreComparison, ScoreComparisonOperator};
use minecraft_command_types::range::IntegerRange;

use crate::compile_context::CompileContext;
use crate::datapack::Datapack;
use crate::typed::arena::TypedAstArena;
use crate::typed::player_score::TypedPlayerScore;

#[derive(Debug, Clone)]
pub enum TypedScoreComparison {
    Range(IntegerRange),
    Score(ScoreComparisonOperator, TypedPlayerScore),
}

impl TypedScoreComparison {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreComparison {
        match self {
            Self::Range(range) => ScoreComparison::Range(range),
            Self::Score(operator, player_score) => {
                ScoreComparison::Score(operator, player_score.compile(arena, datapack, ctx).score)
            }
        }
    }
}
