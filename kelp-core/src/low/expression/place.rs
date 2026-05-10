use minecraft_command_types::{
    command::{
        Command,
        data::DataCommand,
        enums::store_type::StoreType,
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
    },
    nbt_path::NbtPath,
};

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::{
        data_type::resolved::FieldAccessType, environment::value::variable::VariableId,
        expression::resolved::ResolvedExpression,
    },
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedPlaceExpression {
    Variable(VariableId),
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    FieldAccess(Box<Self>, FieldAccessType, String),
    Index(Box<Self>, ResolvedExpression),
}

impl ResolvedPlaceExpression {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> ResolvedExpression {
        match self {
            Self::Variable(id) => datapack.get_variable_value(id),
            Self::Score(score) => ResolvedExpression::Score(score),
            Self::Data(target, path) => ResolvedExpression::Data(Box::new((target, path))),
            Self::FieldAccess(place, access_type, field) => place
                .resolve(datapack)
                .access_field(access_type, field)
                .unwrap(),
            Self::Index(place, index) => place.resolve(datapack).index(&index).unwrap(),
        }
    }

    pub fn assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Variable(id) => {
                datapack.set_variable(id, value);
            }
            Self::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                value.assign_to_data(datapack, ctx, target, path);
            }
            Self::FieldAccess(place, access_type, field) => {
                let old_value = place.clone().resolve(datapack);

                let new_value = old_value.set_field(datapack, ctx, access_type, field, value);

                if let Some(new_value) = new_value {
                    place.assign(datapack, ctx, new_value);
                }
            }
            Self::Index(place, index) => {
                let old_value = place.clone().resolve(datapack);

                let new_value = old_value.set_index(datapack, ctx, &index, value);

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
        value: ResolvedExpression,
    ) {
        match self {
            Self::Variable(_) | Self::FieldAccess(..) | Self::Index(..) => {
                let old_value = self.clone().resolve(datapack);

                let new_value = old_value.perform_arithmetic(datapack, ctx, operator, value);

                self.assign(datapack, ctx, new_value);
            }
            Self::Score(score) => {
                value.operate_on_score(datapack, ctx, score, operator);
            }
            Self::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Get(target.target.clone(), Some(path.clone()), None),
                            )))),
                        ),
                    )),
                );

                value.operate_on_score(datapack, ctx, unique_score.clone(), operator);

                unique_score.assign_to_data(datapack, ctx, target, path);
            }
        }
    }
}
