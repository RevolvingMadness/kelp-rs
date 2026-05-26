use crate::{
    compile_context::CompileContext, data::GeneratedData, datapack::Datapack,
    typed::expression::resolved::Expression, player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl TryFrom<Expression> for RuntimeStorageTarget {
    type Error = ();

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        Ok(match value {
            Expression::Score(score) => Self::Score(score),
            Expression::Data(data) => Self::Data(data),

            _ => return Err(()),
        })
    }
}

impl RuntimeStorageTarget {
    #[must_use]
    pub fn to_expression(self) -> Expression {
        match self {
            Self::Score(score) => Expression::Score(score),
            Self::Data(data) => Expression::Data(data),
        }
    }

    pub fn assign(self, datapack: &mut Datapack, ctx: &mut CompileContext, value: Expression) {
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
