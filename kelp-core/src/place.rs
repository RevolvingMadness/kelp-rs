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
    data_type::DataTypeKind,
    datapack::HighDatapack,
    high::{
        data::GeneratedDataTarget,
        expression::{Expression, ExpressionKind},
        player_score::GeneratedPlayerScore,
    },
    low::expression::ResolvedExpression,
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
    Dereference(Box<ResolvedExpression>),
    Field(Box<ResolvedExpression>, String),
    Index(Box<ResolvedExpression>, Box<ResolvedExpression>),
    Underscore,
}

impl Place {
    pub fn assign_resolved(
        self,
        datapack: &mut HighDatapack,
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
            Self::Variable(name) => {
                datapack.assign_variable(&name, value);
            }
            Self::Underscore => {}
            Self::Tuple(places) => {
                if let ResolvedExpression::Tuple(values) = value {
                    for (place, value) in places.into_iter().zip(values) {
                        place.assign_resolved(datapack, ctx, value);
                    }
                } else {
                    unreachable!("{:?}", value)
                }
            }
            Self::Dereference(expression) => {
                expression.as_place().assign_resolved(datapack, ctx, value);
            }
            Self::Field(mut target, field) => {
                target.assign_field(datapack, ctx, &field, value);
            }
            Self::Index(mut target, index) => {
                target.assign_index(datapack, ctx, *index, value);
            }
        }
    }

    pub fn assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ExpressionKind,
    ) {
        match self {
            Self::Score(score) => {
                let (scale, value) = value.extract_scale();
                let value = value.resolve(datapack, ctx);

                if let Some(scale) = scale {
                    value.assign_to_score_scale(datapack, ctx, score, scale);
                } else {
                    value.assign_to_score(datapack, ctx, score);
                }
            }
            Self::Data(target, path) => {
                let (scale, value) = value.extract_scale();
                let value = value.resolve(datapack, ctx);

                if let Some(scale) = scale {
                    value.assign_to_data_scale(datapack, ctx, target, path, scale);
                } else {
                    value.assign_to_data(datapack, ctx, target, path);
                }
            }
            Self::Variable(name) => {
                let value = value.resolve(datapack, ctx);

                datapack.assign_variable(&name, value);
            }
            Self::Underscore => {
                value.resolve(datapack, ctx);
            }
            Self::Tuple(places) => {
                if let ExpressionKind::Tuple(values) = value {
                    assert!(places.len() == values.len());

                    let safe_values: Vec<ResolvedExpression> = values
                        .into_iter()
                        .map(|value| {
                            let (scale, value) = value.kind.extract_scale();
                            let value = value.resolve(datapack, ctx);

                            match (scale, value) {
                                (Some(scale), value) => {
                                    let unique_score = datapack.get_unique_score();

                                    value.assign_to_score_scale(
                                        datapack,
                                        ctx,
                                        unique_score.clone(),
                                        scale,
                                    );

                                    ResolvedExpression::PlayerScore(unique_score)
                                }
                                (None, ResolvedExpression::PlayerScore(score)) => {
                                    ResolvedExpression::PlayerScore(
                                        score.as_unique_score(datapack, ctx),
                                    )
                                }
                                (None, ResolvedExpression::Data(target_path)) => {
                                    let (target, path) = *target_path;

                                    let (unique_target, unique_path) =
                                        target.as_unique_data(datapack, ctx, path);

                                    ResolvedExpression::Data(Box::new((unique_target, unique_path)))
                                }
                                (None, value) => value,
                            }
                        })
                        .collect();

                    for (place, safe_value) in places.into_iter().zip(safe_values) {
                        place.assign_resolved(datapack, ctx, safe_value);
                    }
                } else {
                    unreachable!("{:?}", value)
                }
            }
            Self::Dereference(expression) => {
                expression.as_place().assign(datapack, ctx, value);
            }
            Self::Field(mut target, field) => {
                let value = value.resolve(datapack, ctx);

                target.assign_field(datapack, ctx, &field, value);
            }
            Self::Index(mut target, index) => {
                let value = value.resolve(datapack, ctx);

                target.assign_index(datapack, ctx, *index, value);
            }
        }
    }

    pub fn augmented_assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Self::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                ResolvedExpression::Data(Box::new((target.clone(), path.clone()))).assign_to_score(
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

                if variable_value.is_lvalue() {
                    variable_value.compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        variable_value.perform_arithmetic(datapack, ctx, operator, value);

                    *datapack.get_variable_mut(&name).unwrap() = new_value;
                }
            }
            Self::Field(mut target, field) => {
                let current_value = target.clone().access_field(&field).unwrap();

                if current_value.is_lvalue() {
                    current_value
                        .as_place()
                        .augmented_assign(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        current_value.perform_arithmetic(datapack, ctx, operator, value);

                    target.assign_field(datapack, ctx, &field, new_value);
                }
            }
            Self::Index(mut target, index) => {
                let current_value = target.clone().index(datapack, ctx, *index.clone()).unwrap();

                if current_value.is_lvalue() {
                    current_value
                        .as_place()
                        .augmented_assign(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        current_value.perform_arithmetic(datapack, ctx, operator, value);

                    target.assign_index(datapack, ctx, *index, new_value);
                }
            }
            Self::Dereference(expression) => {
                expression
                    .as_place()
                    .augmented_assign(datapack, ctx, operator, value);
            }
            Self::Tuple(_) | Self::Underscore => {
                unreachable!()
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
    FieldAccess(Box<PlaceType>, DataTypeKind, String),
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
                if target_data_type.get_index_result().is_none() {
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
            PlaceTypeKind::FieldAccess(target, target_data_type, field) => {
                if target_data_type.get_field_result(ctx, &field).is_none() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeDoesntHaveField {
                                data_type: value_type.clone(),
                                field,
                            },
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
            PlaceTypeKind::Index(target, target_data_type) => {
                if target_data_type.get_index_result().is_none() {
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
            PlaceTypeKind::FieldAccess(target, target_data_type, field) => {
                if target_data_type.get_field_result(ctx, &field).is_none() {
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
            PlaceTypeKind::Underscore => {}
        }

        Some(())
    }
}
