use crate::{
    compile_context::CompileContext, data::GeneratedData, datapack::Datapack,
    semantic::expression::resolved::ResolvedExpression, player_score::GeneratedPlayerScore,
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
                let data = datapack.get_unique_data();

                RuntimeStorageTarget::Data(data)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeStorageTarget {
    Score(GeneratedPlayerScore),
    Data(GeneratedData),
}

impl TryFrom<ResolvedExpression> for RuntimeStorageTarget {
    type Error = ();

    fn try_from(value: ResolvedExpression) -> Result<Self, Self::Error> {
        Ok(match value {
            ResolvedExpression::Score(score) => Self::Score(score),
            ResolvedExpression::Data(data) => Self::Data(data),

            _ => return Err(()),
        })
    }
}

impl RuntimeStorageTarget {
    #[must_use]
    pub fn to_expression(self) -> ResolvedExpression {
        match self {
            Self::Score(score) => ResolvedExpression::Score(score),
            Self::Data(data) => ResolvedExpression::Data(data),
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
            Self::Data(data) => {
                value.assign_to_data(datapack, ctx, data);
            }
        }
    }

    #[inline]
    pub fn assign_target(self, datapack: &mut Datapack, ctx: &mut CompileContext, value: Self) {
        self.assign(datapack, ctx, value.to_expression());
    }
}
