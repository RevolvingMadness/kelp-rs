use minecraft_command_types::{
    command::{
        Command,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::{
        environment::value::{
            ValueDeclaration, ValueDeclarationKind, ValueId, variable::VariableId,
        },
        expression::resolved::ResolvedExpression,
    },
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
};

#[derive(Debug, Clone)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(Box<GeneratedDataTarget>, NbtPath),
    Value(ValueId),
    Tuple(Vec<Self>),
    Underscore,
}

impl Place {
    pub fn dereference(self, datapack: &mut Datapack) -> Option<Self> {
        Some(match self {
            Self::Score(score) => Self::Score(score),
            Self::Data(target, path) => Self::Data(target, path),
            Self::Value(id) => {
                let declaration = datapack.get_value(id);

                match &declaration.kind {
                    ValueDeclarationKind::Variable(..) => {
                        let (_, value) = datapack.get_variable_value(VariableId(id.0));

                        value.clone().as_place()?
                    }
                    ValueDeclarationKind::Function(..) => return None,
                }
            }
            Self::Tuple(..) | Self::Underscore => return None,
        })
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
                value.assign_to_data(datapack, ctx, *target, path);
            }
            Self::Value(id) => {
                let ValueDeclaration {
                    kind: ValueDeclarationKind::Variable(..),
                    ..
                } = datapack.get_value(id)
                else {
                    unreachable!("The expression '{:?}' cannot be assigned to", self);
                };

                datapack.set_variable(VariableId(id.0), value);
            }
            Self::Underscore => {}
            Self::Tuple(places) => {
                let ResolvedExpression::Tuple(expressions) = value else {
                    unreachable!();
                };

                assert!(places.len() == expressions.len());

                for (place, expression) in places.into_iter().zip(expressions) {
                    place.assign(datapack, ctx, expression);
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
            Self::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Self::Data(original_target, path) => {
                let target = original_target.target.clone();

                let unique_score = datapack.get_unique_score();

                ResolvedExpression::Data(Box::new((*original_target, path.clone())))
                    .assign_to_score(datapack, ctx, unique_score.clone());

                unique_score
                    .clone()
                    .assign_augmented(datapack, ctx, operator, value);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target,
                            path,
                            NumericSNBTType::Integer,
                            NotNan::new(1.0).unwrap(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Scoreboard(
                                ScoreboardCommand::Players(PlayersScoreboardCommand::Get(
                                    unique_score.score,
                                )),
                            )))),
                        ),
                    )),
                );
            }
            Self::Value(id) => {
                let ValueDeclaration {
                    kind: ValueDeclarationKind::Variable(..),
                    ..
                } = datapack.get_value(id)
                else {
                    return;
                };

                let id = VariableId(id.0);

                let variable_value = datapack.get_variable_value(id).1.clone();

                if variable_value.is_lvalue() {
                    variable_value.compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        variable_value.perform_arithmetic(datapack, ctx, operator, value);

                    datapack.set_variable(id, new_value);
                }
            }
            Self::Tuple(..) | Self::Underscore => {
                unreachable!()
            }
        }
    }
}
