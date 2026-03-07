use minecraft_command_types::{
    command::{
        Command,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::NbtPath,
    snbt::SNBTString,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{
        Expression, ExpressionKind,
        constant::{ConstantExpression, ConstantExpressionKind},
    },
    high::{data::GeneratedDataTarget, player_score::GeneratedPlayerScore},
    operator::ArithmeticOperator,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    Variable(String),
    Tuple(Vec<Self>),
    Dereference(Box<ConstantExpression>),
    Field(Box<ConstantExpression>, SNBTString),
    Index(Box<ConstantExpression>, Box<ConstantExpression>),
    Underscore,
}

impl Place {
    pub fn assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ConstantExpression,
    ) {
        match self {
            Self::Score(score) => {
                value.kind.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                value.kind.assign_to_data(datapack, ctx, target, path);
            }
            Self::Variable(name) => {
                datapack.assign_variable(&name, value);
            }
            Self::Underscore => {}
            Self::Tuple(places) => {
                if let ConstantExpressionKind::Tuple(values) = value.kind {
                    if places.len() != values.len() {
                        return;
                    }

                    let safe_values: Vec<ConstantExpression> = values
                        .into_iter()
                        .map(|v| match v.kind {
                            ConstantExpressionKind::PlayerScore(_) => {
                                let unique = v.kind.as_score(datapack, ctx, true);
                                ConstantExpressionKind::PlayerScore(unique)
                                    .into_dummy_constant_expression()
                            }
                            ConstantExpressionKind::Data(_, _) => {
                                let (t, p) = v.kind.as_data_force(datapack, ctx);
                                ConstantExpressionKind::Data(t, p).into_dummy_constant_expression()
                            }
                            _ => v,
                        })
                        .collect();

                    for (place, value) in places.into_iter().zip(safe_values) {
                        place.assign(datapack, ctx, value);
                    }
                } else {
                    unreachable!()
                }
            }
            Self::Dereference(expression) => {
                expression.kind.as_place().assign(datapack, ctx, value);
            }
            Self::Field(expression, field) => {
                expression
                    .kind
                    .access_field(field.1)
                    .as_place()
                    .assign(datapack, ctx, value);
            }
            Self::Index(mut target, index) => {
                target.kind.assign_index(datapack, ctx, index.kind, value);
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    ) {
        match self {
            Self::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Self::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                ConstantExpressionKind::Data(target.clone(), path.clone()).assign_to_score(
                    datapack,
                    ctx,
                    unique_score.clone(),
                );

                unique_score
                    .clone()
                    .assign_augmented(datapack, ctx, operator, value);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target.target,
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
            Self::Variable(name) => {
                let (_, variable_value) = datapack.get_variable(&name).unwrap();

                if variable_value.kind.is_lvalue() {
                    variable_value
                        .kind
                        .compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_kind = variable_value
                        .kind
                        .perform_arithmetic(datapack, ctx, operator, value);

                    datapack.get_variable_mut(&name).unwrap().kind = new_kind;
                }
            }
            Self::Field(expression, field) => {
                expression
                    .kind
                    .access_field(field.1)
                    .as_place()
                    .augmented_assign(datapack, ctx, operator, value);
            }
            Self::Index(mut target, index) => {
                let current_value = target.kind.clone().index(datapack, ctx, index.kind.clone());

                if current_value.is_lvalue() {
                    current_value
                        .as_place()
                        .augmented_assign(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        current_value.perform_arithmetic(datapack, ctx, operator, value);
                    target.kind.assign_index(
                        datapack,
                        ctx,
                        index.kind,
                        new_value.into_dummy_constant_expression(),
                    );
                }
            }
            Self::Tuple(_) | Self::Underscore => {
                unreachable!()
            }
            Self::Dereference(expression) => {
                expression
                    .kind
                    .as_place()
                    .augmented_assign(datapack, ctx, operator, value);
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceTypeKind {
    Score(DataTypeKind),
    Data(DataTypeKind),
    Tuple(Vec<PlaceType>, DataTypeKind),
    Variable(DataTypeKind),
    Index(Box<PlaceType>, DataTypeKind),
    Underscore,
}

impl PlaceTypeKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> PlaceType {
        PlaceType { span, kind: self }
    }
}

#[derive(Debug)]
pub struct PlaceType {
    pub span: Span,
    pub kind: PlaceTypeKind,
}

impl PlaceType {
    #[must_use]
    pub fn get_index_result(&self) -> Option<DataTypeKind> {
        match &self.kind {
            PlaceTypeKind::Data(data_type_kind) | PlaceTypeKind::Variable(data_type_kind) => {
                data_type_kind.get_index_result()
            }
            PlaceTypeKind::Index(place_type, _) => place_type.get_index_result(),
            PlaceTypeKind::Score(_) | PlaceTypeKind::Tuple(_, _) | PlaceTypeKind::Underscore => {
                None
            }
        }
    }

    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value: Expression,
        value_type: &DataTypeKind,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Score(_) => {
                if let Some(result) = value_type.is_score_compatible(ctx)
                    && !result
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotScoreCompatible(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceTypeKind::Data(_) | PlaceTypeKind::Underscore => {}
            PlaceTypeKind::Tuple(place_types, _) => {
                let ExpressionKind::Tuple(expressions) = value.kind else {
                    unreachable!();
                };

                assert!(expressions.len() == place_types.len());

                place_types
                    .into_iter()
                    .zip(expressions)
                    .map(|(place_type, value)| {
                        let value_type = value.kind.infer_data_type(ctx)?;

                        place_type.perform_assignment_semantic_analysis(ctx, value, &value_type)
                    })
                    .all_some()?;
            }
            PlaceTypeKind::Variable(data_type) => {
                if !value_type.equals(&data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type,
                                actual: value_type.clone(),
                            },
                        ),
                    });
                }

                if !data_type.is_runtime() && ctx.loop_depth != 0 {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                        ),
                    });
                }
            }
            PlaceTypeKind::Index(target, target_data_type) => {
                if target.get_index_result().is_none() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeIndexed(value_type.clone()),
                        ),
                    });
                }

                if !target_data_type.is_runtime() && ctx.loop_depth != 0 {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                        ),
                    });
                }
            }
        }

        Some(())
    }

    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        operator: &ArithmeticOperator,
        value: &Expression,
        value_type: &DataTypeKind,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Data(data_type) | PlaceTypeKind::Score(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::InvalidAugmentedAssignmentType(
                                *operator,
                                data_type,
                                value_type.clone(),
                            ),
                        ),
                    });
                }
            }
            PlaceTypeKind::Variable(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::InvalidAugmentedAssignmentType(
                                *operator,
                                data_type,
                                value_type.clone(),
                            ),
                        ),
                    });
                }

                if !data_type.is_runtime() && ctx.loop_depth != 0 {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                        ),
                    });
                }
            }
            PlaceTypeKind::Tuple(_, data_type) => {
                return ctx.add_info(SemanticAnalysisInfo {
                    span: self.span,
                    kind: SemanticAnalysisInfoKind::Error(
                        SemanticAnalysisError::CannotPerformAugmentedAssignment(data_type),
                    ),
                });
            }
            PlaceTypeKind::Index(target, _) => {
                if target.get_index_result().is_none() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeIndexed(value_type.clone()),
                        ),
                    });
                }
            }
            PlaceTypeKind::Underscore => {}
        }

        Some(())
    }
}
