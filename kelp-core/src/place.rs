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
    low::expression::Expression,
    middle::{
        data_type::DataTypeKind,
        expression::{Expression as MiddleExpression, ExpressionKind},
    },
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
    semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
    span::Span,
    trait_ext::{CollectOptionAllIterExt, FilterOptionIteratorExt},
};

#[derive(Debug)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    Variable(String),
    Tuple(Vec<Self>),
    Dereference(Box<Expression>),
    Field(Box<Expression>, String),
    Index(Box<Expression>, Box<Expression>),
    Underscore,
}

impl Place {
    pub fn assign_resolved(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value: Expression,
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
                if let Expression::Tuple(values) = value {
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

    pub fn assign(self, datapack: &mut Datapack, ctx: &mut CompileContext, value: ExpressionKind) {
        match self {
            Self::Score(score) => {
                let value = value.resolve(datapack, ctx);

                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                let value = value.resolve(datapack, ctx);

                value.assign_to_data(datapack, ctx, target, path);
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

                    let safe_values: Vec<Expression> = values
                        .into_iter()
                        .map(|value| {
                            let value = value.kind.resolve(datapack, ctx);

                            match value {
                                Expression::PlayerScore(score) => {
                                    Expression::PlayerScore(score.as_unique_score(datapack, ctx))
                                }
                                Expression::Data(target_path) => {
                                    let (target, path) = *target_path;

                                    let (unique_target, unique_path) =
                                        target.as_unique_data(datapack, ctx, path);

                                    Expression::Data(Box::new((unique_target, unique_path)))
                                }
                                _ => value,
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
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: Expression,
    ) {
        match self {
            Self::Score(score) => {
                score.assign_augmented(datapack, ctx, operator, value);
            }
            Self::Data(target, path) => {
                let unique_score = datapack.get_unique_score();

                Expression::Data(Box::new((target.clone(), path.clone()))).assign_to_score(
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
    Tuple(Vec<PlaceType>),
    Variable(DataTypeKind),
    Index(Box<PlaceType>),
    FieldAccess(Box<PlaceType>, String),
    Underscore,
}

impl PlaceTypeKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> PlaceType {
        PlaceType { span, kind: self }
    }

    #[must_use]
    pub fn get_data_type(self, ctx: &SemanticAnalysisContext) -> Option<DataTypeKind> {
        Some(match self {
            Self::Score(data_type) => DataTypeKind::Score(Box::new(data_type)),
            Self::Data(data_type) => DataTypeKind::Data(Box::new(data_type)),
            Self::Tuple(place_types) => DataTypeKind::Tuple(
                place_types
                    .into_iter()
                    .map(|place_type| place_type.kind.get_data_type(ctx))
                    .collect::<Option<_>>()?,
            ),
            Self::Variable(data_type) => data_type,
            Self::Index(place_type) => place_type.kind.get_index_result(ctx)?,
            Self::FieldAccess(place_type, field) => {
                place_type.kind.get_field_result(ctx, &field)?
            }
            Self::Underscore => DataTypeKind::Inferred,
        })
    }

    #[must_use]
    pub fn get_index_result(&self, ctx: &SemanticAnalysisContext) -> Option<DataTypeKind> {
        match self {
            Self::Variable(data_type_kind) => data_type_kind.get_index_result(),
            Self::Index(place_type) => place_type.kind.get_index_result(ctx)?.get_index_result(),
            Self::FieldAccess(place_type, field) => place_type
                .kind
                .get_field_result(ctx, field)?
                .get_index_result(),
            Self::Score(_) | Self::Data(_) | Self::Tuple(_) | Self::Underscore => None,
        }
    }

    #[must_use]
    pub fn get_field_result(
        &self,
        ctx: &SemanticAnalysisContext,
        field: &str,
    ) -> Option<DataTypeKind> {
        match self {
            Self::Score(data_type) | Self::Data(data_type) | Self::Variable(data_type) => {
                data_type.get_field_result(ctx, field)
            }
            Self::Index(place_type) => place_type
                .kind
                .get_index_result(ctx)?
                .get_field_result(ctx, field),
            Self::FieldAccess(place_type, field) => place_type
                .kind
                .get_field_result(ctx, field)?
                .get_field_result(ctx, field),
            Self::Tuple(_) | Self::Underscore => None,
        }
    }

    #[must_use]
    pub fn is_runtime(&self, ctx: &SemanticAnalysisContext) -> Option<bool> {
        Some(match self {
            Self::Score(_) | Self::Data(_) => true,
            Self::Tuple(place_types) => place_types
                .iter()
                .filter_all(|place_type| place_type.kind.is_runtime(ctx))?,
            Self::Variable(data_type) => data_type.is_runtime(),
            Self::Index(place_type) => place_type
                .kind
                .get_index_result(ctx)
                .is_some_and(|data_type| data_type.is_runtime()),
            Self::FieldAccess(place_type, field) => {
                place_type.kind.get_field_result(ctx, field)?.is_runtime()
            }
            Self::Underscore => false,
        })
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
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Score(_) => {
                if let Some(result) = value.data_type.is_score_compatible(ctx)
                    && !result
                {
                    return ctx.add_error_ret(
                        value_span,
                        SemanticAnalysisError::TypeIsNotScoreCompatible(value.data_type.clone()),
                    );
                }
            }
            PlaceTypeKind::Data(_) | PlaceTypeKind::Underscore => {}
            PlaceTypeKind::Tuple(place_types) => {
                let ExpressionKind::Tuple(expressions) = &value.kind else {
                    unreachable!();
                };

                assert!(expressions.len() == place_types.len());

                place_types
                    .into_iter()
                    .zip(expressions)
                    .map(|(place_type, value)| {
                        place_type.perform_assignment_semantic_analysis(ctx, value_span, value)
                    })
                    .run_all_succeeded()?;
            }
            PlaceTypeKind::Variable(data_type) => {
                if !value.data_type.equals(&data_type) {
                    return ctx.add_error_ret(
                        value_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: data_type,
                            actual: value.data_type.clone(),
                        },
                    );
                }

                if !data_type.is_runtime() && ctx.loop_depth != 0 {
                    return ctx.add_error_ret(
                        self.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            PlaceTypeKind::Index(target) => {
                if target.kind.get_index_result(ctx).is_none() {
                    let target_data_type = target.kind.get_data_type(ctx)?;

                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::CannotBeIndexed(target_data_type),
                    );
                }

                if ctx.loop_depth != 0
                    && let Some(is_runtime) = target.kind.is_runtime(ctx)
                    && !is_runtime
                {
                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            PlaceTypeKind::FieldAccess(target, field) => {
                if target.kind.get_field_result(ctx, &field).is_none() {
                    let target_data_type = target.kind.get_data_type(ctx)?;

                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::TypeDoesntHaveField {
                            data_type: target_data_type,
                            field,
                        },
                    );
                }

                if ctx.loop_depth != 0
                    && let Some(is_runtime) = target.kind.is_runtime(ctx)
                    && !is_runtime
                {
                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
        }

        Some(())
    }

    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        operator: ArithmeticOperator,
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        match self.kind {
            PlaceTypeKind::Data(data_type) | PlaceTypeKind::Score(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error_ret(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            data_type,
                            value.data_type.clone(),
                        ),
                    );
                }
            }
            PlaceTypeKind::Variable(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error_ret(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            data_type,
                            value.data_type.clone(),
                        ),
                    );
                }

                if !data_type.is_runtime() && ctx.loop_depth != 0 {
                    return ctx.add_error_ret(
                        self.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            PlaceTypeKind::Tuple(_) | PlaceTypeKind::Underscore => {
                let self_data_type = self.kind.get_data_type(ctx)?;

                return ctx.add_error_ret(
                    self.span,
                    SemanticAnalysisError::CannotPerformAugmentedAssignment(self_data_type),
                );
            }
            PlaceTypeKind::Index(target) => {
                if target.kind.get_index_result(ctx).is_none() {
                    return ctx.add_error_ret(
                        self.span,
                        SemanticAnalysisError::CannotBeIndexed(value.data_type.clone()),
                    );
                }

                if ctx.loop_depth != 0
                    && let Some(is_runtime) = target.kind.is_runtime(ctx)
                    && !is_runtime
                {
                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            PlaceTypeKind::FieldAccess(target, field) => {
                if target.kind.get_field_result(ctx, &field).is_none() {
                    return ctx.add_error_ret(
                        self.span,
                        SemanticAnalysisError::CannotBeIndexed(value.data_type.clone()),
                    );
                }

                if ctx.loop_depth != 0
                    && let Some(is_runtime) = target.kind.is_runtime(ctx)
                    && !is_runtime
                {
                    return ctx.add_error_ret(
                        target.span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
        }

        Some(())
    }
}
