use minecraft_command_types::nbt_path::NbtPath;

use crate::{
    compile_context::CompileContext, data::GeneratedDataTarget, datapack::Datapack,
    low::expression::resolved::ResolvedExpression, player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone)]
pub enum RuntimeStorageType {
    Score,
    Data,
}

impl RuntimeStorageType {
    #[must_use]
    pub fn instantiate(self, datapack: &mut Datapack) -> RuntimeStorageTarget {
        match self {
            Self::Score => {
                let unique_score = datapack.get_unique_score();

                RuntimeStorageTarget::Score(unique_score)
            }
            Self::Data => {
                let (target, path) = datapack.get_unique_data();

                RuntimeStorageTarget::Data(target, path)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeStorageTarget {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
}

impl RuntimeStorageTarget {
    #[must_use]
    pub fn to_expression(self) -> ResolvedExpression {
        match self {
            Self::Score(score) => ResolvedExpression::PlayerScore(score),
            Self::Data(target, path) => ResolvedExpression::Data(Box::new((target, path))),
        }
    }

    pub fn assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                value.assign_to_data(datapack, ctx, target, path);
            }
        }
    }
}
