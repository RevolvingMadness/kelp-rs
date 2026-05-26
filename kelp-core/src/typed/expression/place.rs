use crate::{
    compile_context::CompileContext,
    data::GeneratedData,
    datapack::Datapack,
    low::environment::value::variable::VariableId,
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
    typed::{data_type::resolved::FieldAccessType, expression::Expression},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypedPlaceExpression {
    Variable(VariableId),
    Score(GeneratedPlayerScore),
    Data(GeneratedData),
    FieldAccess(Box<Self>, FieldAccessType, String),
    Index(Box<Self>, Expression),
}

impl TypedPlaceExpression {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Expression {
        match self {
            Self::Variable(id) => datapack.get_variable_value(id),
            Self::Score(score) => Expression::Score(score),
            Self::Data(data) => Expression::Data(data),
            Self::FieldAccess(place, access_type, field) => place
                .resolve(datapack, ctx)
                .access_field(access_type, &field)
                .unwrap(),
            Self::Index(place, index) => place.resolve(datapack, ctx).index(ctx, index).unwrap(),
        }
    }

    pub fn assign(self, datapack: &mut Datapack, ctx: &mut CompileContext, value: Expression) {
        match self {
            Self::Variable(id) => {
                datapack.set_variable(id, value);
            }
            Self::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(data) => {
                value.assign_to_data(datapack, ctx, data);
            }
            Self::FieldAccess(place, access_type, field) => {
                let old_value = place.clone().resolve(datapack, ctx);

                let new_value = old_value.set_field(datapack, ctx, access_type, field, value);

                if let Some(new_value) = new_value {
                    place.assign(datapack, ctx, new_value);
                }
            }
            Self::Index(place, index) => {
                let old_value = place.clone().resolve(datapack, ctx);

                let new_value = old_value.set_index(datapack, ctx, index, value);

                if let Some(new_value) = new_value {
                    place.assign(datapack, ctx, new_value);
                }
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: Expression,
    ) {
        match self {
            Self::Variable(_) | Self::FieldAccess(..) | Self::Index(..) => {
                let old_value = self.clone().resolve(datapack, ctx);

                let new_value = old_value.augmented_assign(datapack, ctx, operator, value);

                if let Some(new_value) = new_value {
                    self.assign(datapack, ctx, new_value);
                }
            }
            Self::Score(score) => {
                value.operate_on_score(datapack, ctx, score, operator);
            }
            Self::Data(data) => {
                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    data.clone()
                        .get()
                        .run()
                        .store_result_score(unique_score.score.clone()),
                );

                value.operate_on_score(datapack, ctx, unique_score.clone(), operator);

                unique_score.assign_to_data(datapack, ctx, data);
            }
        }
    }
}
