use minecraft_command_types::{
    command::{
        Command,
        enums::{numeric_snbt_type::NumericSNBTType, store_type::StoreType},
        execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    nbt_path::{NbtPath, NbtPathNode},
    snbt::SNBTString,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    high::semantic_analysis_context::{
        SemanticAnalysisContext, info::error::SemanticAnalysisError,
    },
    low::expression::Expression,
    middle::{
        data_type::DataType,
        environment::{
            Environment,
            value::{ValueDeclaration, ValueId},
        },
        expression::{Expression as MiddleExpression, ExpressionKind},
    },
    operator::ArithmeticOperator,
    player_score::GeneratedPlayerScore,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum Place {
    Score(GeneratedPlayerScore),
    Data(GeneratedDataTarget, NbtPath),
    Value(ValueId),
    Tuple(Vec<Self>),
    Dereference(Box<Self>),
    Field(Box<Self>, String),
    Index(Box<Self>, Box<Expression>),
    Underscore,
}

impl Place {
    pub fn dereference(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Option<Self> {
        Some(match self {
            Self::Score(score) => Self::Score(score),
            Self::Data(target, path) => Self::Data(target, path),
            Self::Value(id) => datapack.get_variable_value(id).1.clone().as_place()?,
            Self::Dereference(place) => place
                .dereference(datapack, ctx)?
                .dereference(datapack, ctx)?,
            Self::Field(expression, field) => expression
                .access_field(datapack, ctx, &field)?
                .dereference(datapack, ctx)?,
            Self::Index(target, index) => target
                .index(datapack, ctx, *index)?
                .dereference(datapack, ctx)?,
            Self::Tuple(_) | Self::Underscore => return None,
        })
    }

    #[must_use]
    pub fn access_field(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        field: &str,
    ) -> Option<Self> {
        Some(match self {
            Self::Data(target, path) => Self::Data(
                target,
                path.with_node(NbtPathNode::Named(
                    SNBTString(false, field.to_owned()),
                    None,
                )),
            ),
            Self::Value(id) => datapack
                .get_variable_value(id)
                .1
                .clone()
                .as_place()?
                .access_field(datapack, ctx, field)?,
            Self::Tuple(mut places) => {
                let index = field.parse::<i32>().ok()?;

                places.swap_remove(index as usize)
            }
            Self::Dereference(place) => place
                .dereference(datapack, ctx)?
                .access_field(datapack, ctx, field)?,
            Self::Field(place, inner_field) => place
                .access_field(datapack, ctx, &inner_field)?
                .access_field(datapack, ctx, field)?,
            Self::Index(place, index) => place
                .index(datapack, ctx, *index)?
                .access_field(datapack, ctx, field)?,
            Self::Score(_) | Self::Underscore => return None,
        })
    }

    #[must_use]
    pub fn index(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        index: Expression,
    ) -> Option<Self> {
        Some(match self {
            Self::Data(target, path) => Self::Data(
                target,
                path.with_node(NbtPathNode::Index(Some(index.as_snbt_macros(ctx)))),
            ),
            Self::Value(id) => datapack
                .get_variable_value(id)
                .1
                .clone()
                .as_place()?
                .index(datapack, ctx, index)?,
            Self::Dereference(place) => place
                .dereference(datapack, ctx)?
                .index(datapack, ctx, index)?,
            Self::Field(place, inner_field) => place
                .access_field(datapack, ctx, &inner_field)?
                .index(datapack, ctx, index)?,
            Self::Index(place, inner_index) => place
                .index(datapack, ctx, *inner_index)?
                .index(datapack, ctx, index)?,
            Self::Score(_) | Self::Tuple(_) | Self::Underscore => return None,
        })
    }

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
            Self::Value(id) => {
                datapack.set_variable(id, value);
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
            Self::Dereference(place) => {
                place
                    .dereference(datapack, ctx)
                    .unwrap()
                    .assign_resolved(datapack, ctx, value);
            }
            Self::Field(target, field) => {
                target
                    .access_field(datapack, ctx, &field)
                    .unwrap()
                    .assign_resolved(datapack, ctx, value);
            }
            Self::Index(target, index) => {
                target
                    .index(datapack, ctx, *index)
                    .unwrap()
                    .assign_resolved(datapack, ctx, value);
            }
        }
    }

    pub fn assign(self, datapack: &mut Datapack, ctx: &mut CompileContext, value: Expression) {
        match self {
            Self::Score(score) => {
                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target, path) => {
                value.assign_to_data(datapack, ctx, target, path);
            }
            Self::Value(id) => {
                datapack.set_variable(id, value);
            }
            Self::Underscore => {}
            Self::Tuple(places) => {
                if let Expression::Tuple(values) = value {
                    assert!(places.len() == values.len());

                    let safe_values: Vec<Expression> = values
                        .into_iter()
                        .map(|value| match value {
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
                        })
                        .collect();

                    for (place, safe_value) in places.into_iter().zip(safe_values) {
                        place.assign_resolved(datapack, ctx, safe_value);
                    }
                } else {
                    unreachable!("{:?}", value)
                }
            }
            Self::Dereference(place) => {
                place
                    .dereference(datapack, ctx)
                    .unwrap()
                    .assign(datapack, ctx, value);
            }
            Self::Field(target, field) => {
                target
                    .access_field(datapack, ctx, &field)
                    .unwrap()
                    .assign(datapack, ctx, value);
            }
            Self::Index(target, index) => {
                target
                    .index(datapack, ctx, *index)
                    .unwrap()
                    .assign(datapack, ctx, value);
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
            Self::Value(id) => {
                let variable_value = datapack.get_variable_value(id).1.clone();

                if variable_value.is_lvalue() {
                    variable_value.compile_augmented_assignment(datapack, ctx, operator, value);
                } else {
                    let new_value =
                        variable_value.perform_arithmetic(datapack, ctx, operator, value);

                    datapack.set_variable(id, new_value);
                }
            }
            Self::Field(target, field) => {
                let target = target.access_field(datapack, ctx, &field).unwrap();

                target.augmented_assign(datapack, ctx, operator, value);
            }
            Self::Index(target, index) => {
                let target = target.index(datapack, ctx, *index).unwrap();

                target.augmented_assign(datapack, ctx, operator, value);
            }
            Self::Dereference(place) => {
                let place = place.dereference(datapack, ctx).unwrap();

                place.augmented_assign(datapack, ctx, operator, value);
            }
            Self::Tuple(_) | Self::Underscore => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug)]
pub enum PlaceTypeKind {
    Score(DataType),
    Data(DataType),
    Tuple(Vec<PlaceType>),
    Value(ValueId),
    Index(Box<PlaceType>),
    FieldAccess(Box<PlaceType>, String),
    Dereference(Box<PlaceType>),
    Underscore,
}

impl PlaceTypeKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> PlaceType {
        PlaceType { span, kind: self }
    }

    #[must_use]
    pub fn get_data_type(&self, environment: &Environment) -> Option<DataType> {
        Some(match self {
            Self::Score(data_type) => DataType::Score(Box::new(data_type.clone())),
            Self::Data(data_type) => DataType::Data(Box::new(data_type.clone())),
            Self::Tuple(place_types) => DataType::Tuple(
                place_types
                    .iter()
                    .map(|place_type| place_type.kind.get_data_type(environment))
                    .collect::<Option<_>>()?,
            ),
            Self::Value(id) => {
                let ValueDeclaration::Variable(declaration) = environment.get_value(*id);

                declaration.data_type.as_ref()?.clone()
            }
            Self::Index(place_type) => place_type.kind.get_index_result(environment)?,
            Self::FieldAccess(place_type, field) => {
                place_type.kind.get_field_result(environment, field)?
            }
            Self::Dereference(place_type) => place_type
                .kind
                .get_data_type(environment)?
                .dereference()
                .ok()?,
            Self::Underscore => DataType::Inferred,
        })
    }

    #[must_use]
    pub fn get_index_result(&self, environment: &Environment) -> Option<DataType> {
        match self {
            Self::Value(id) => {
                let ValueDeclaration::Variable(declaration) = environment.get_value(*id);

                declaration.data_type.as_ref()?.clone().get_index_result()
            }
            Self::Index(place_type) => place_type
                .kind
                .get_index_result(environment)?
                .get_index_result(),
            Self::FieldAccess(place_type, field) => place_type
                .kind
                .get_field_result(environment, field)?
                .get_index_result(),
            Self::Dereference(place_type) => place_type
                .kind
                .get_data_type(environment)?
                .get_dereferenced_result()?
                .get_index_result(),
            Self::Score(_) | Self::Data(_) | Self::Tuple(_) | Self::Underscore => None,
        }
    }

    #[must_use]
    pub fn get_field_result(&self, environment: &Environment, field: &str) -> Option<DataType> {
        match self {
            Self::Score(data_type) | Self::Data(data_type) => {
                data_type.get_field_result(environment, field)
            }
            Self::Value(id) => {
                let ValueDeclaration::Variable(declaration) = environment.get_value(*id);

                declaration
                    .data_type
                    .as_ref()?
                    .clone()
                    .get_field_result(environment, field)
            }
            Self::Index(place_type) => place_type
                .kind
                .get_index_result(environment)?
                .get_field_result(environment, field),
            Self::FieldAccess(place_type, field) => place_type
                .kind
                .get_field_result(environment, field)?
                .get_field_result(environment, field),
            Self::Dereference(place_type) => place_type
                .kind
                .get_data_type(environment)?
                .dereference()
                .ok()?
                .get_field_result(environment, field),
            Self::Tuple(_) | Self::Underscore => None,
        }
    }

    pub fn perform_assignment_semantic_analysis(
        self,
        self_span: Span,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        match self {
            Self::Score(_) => {
                if let Some(result) = value.data_type.is_score_compatible(&ctx.environment)
                    && !result
                {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::TypeIsNotScoreCompatible(value.data_type.clone()),
                    );
                }
            }
            Self::Data(_) | Self::Underscore => {}
            Self::Tuple(place_types) => {
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
            Self::Value(id) => {
                let ValueDeclaration::Variable(declaration) = ctx.get_value(id);

                let data_type = declaration.data_type.as_ref()?;

                if !value.data_type.equals(data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: data_type.clone(),
                            actual: value.data_type.clone(),
                        },
                    );
                }

                if ctx.loop_depth != 0 && data_type.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            Self::Index(target) => {
                let Some(index_result) = target.kind.get_index_result(&ctx.environment) else {
                    let target_data_type = target.kind.get_data_type(&ctx.environment)?;

                    return ctx.add_error(
                        target.span,
                        SemanticAnalysisError::CannotBeIndexed(target_data_type),
                    );
                };

                if ctx.loop_depth != 0 && index_result.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
            Self::Dereference(place_type) => {
                let place_data_type = place_type.kind.get_data_type(&ctx.environment)?;

                match place_data_type {
                    DataType::Score(_) => {
                        if let Some(result) = value.data_type.is_score_compatible(&ctx.environment)
                            && !result
                        {
                            return ctx.add_error(
                                value_span,
                                SemanticAnalysisError::TypeIsNotScoreCompatible(
                                    value.data_type.clone(),
                                ),
                            );
                        }
                    }

                    DataType::Data(_) => {}

                    DataType::Reference(inner_type) => {
                        if !value.data_type.equals(&inner_type) {
                            return ctx.add_error(
                                value_span,
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: *inner_type,
                                    actual: value.data_type.clone(),
                                },
                            );
                        }

                        if ctx.loop_depth != 0 && inner_type.is_compiletime(&ctx.environment) {
                            return ctx.add_error(
                                self_span,
                                SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                            );
                        }
                    }

                    _ => {
                        return ctx.add_error(
                            place_type.span,
                            SemanticAnalysisError::CannotBeDereferenced(place_data_type),
                        );
                    }
                }
            }
            Self::FieldAccess(target, field) => {
                let Some(field_result) = target.kind.get_field_result(&ctx.environment, &field)
                else {
                    let target_data_type = target.kind.get_data_type(&ctx.environment)?;

                    return ctx.add_error(
                        target.span,
                        SemanticAnalysisError::TypeDoesntHaveField {
                            data_type: target_data_type,
                            field,
                        },
                    );
                };

                if ctx.loop_depth != 0 && field_result.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }
            }
        }

        Some(())
    }

    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        self_span: Span,
        ctx: &mut SemanticAnalysisContext,
        operator: ArithmeticOperator,
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        match self {
            Self::Data(data_type) | Self::Score(data_type) => {
                if !data_type.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            data_type,
                            value.data_type.clone(),
                        ),
                    );
                }
            }
            Self::Value(id) => {
                let ValueDeclaration::Variable(declaration) = ctx.get_value(id);

                let data_type = declaration.data_type.as_ref()?;

                if ctx.loop_depth != 0 && data_type.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }

                if !data_type.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            data_type.clone(),
                            value.data_type.clone(),
                        ),
                    );
                }
            }
            Self::Tuple(_) | Self::Underscore => {
                let self_data_type = self.get_data_type(&ctx.environment)?;

                return ctx.add_error(
                    self_span,
                    SemanticAnalysisError::CannotPerformAugmentedAssignment(self_data_type),
                );
            }
            Self::Index(target) => {
                let Some(index_result) = target.kind.get_index_result(&ctx.environment) else {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CannotBeIndexed(value.data_type.clone()),
                    );
                };

                if ctx.loop_depth != 0 && index_result.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }

                if !index_result.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            index_result,
                            value.data_type.clone(),
                        ),
                    );
                }
            }
            Self::Dereference(place_type) => {
                let place_data_type = place_type.kind.get_data_type(&ctx.environment)?;

                if !place_data_type.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            place_data_type,
                            value.data_type.clone(),
                        ),
                    );
                }
            }
            Self::FieldAccess(target, field) => {
                let Some(field_result) = target.kind.get_field_result(&ctx.environment, &field)
                else {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::TypeDoesntHaveField {
                            data_type: value.data_type.clone(),
                            field,
                        },
                    );
                };

                if ctx.loop_depth != 0 && field_result.is_compiletime(&ctx.environment) {
                    return ctx.add_error(
                        self_span,
                        SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop,
                    );
                }

                if !field_result.can_perform_augmented_assignment(operator, &value.data_type) {
                    return ctx.add_error(
                        value_span,
                        SemanticAnalysisError::InvalidAugmentedAssignmentType(
                            operator,
                            field_result,
                            value.data_type.clone(),
                        ),
                    );
                }
            }
        }

        Some(())
    }
}

#[derive(Debug)]
pub struct PlaceType {
    pub span: Span,
    pub kind: PlaceTypeKind,
}

impl PlaceType {
    #[inline]
    pub fn perform_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        self.kind
            .perform_assignment_semantic_analysis(self.span, ctx, value_span, value)
    }

    #[inline]
    pub fn perform_augmented_assignment_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        operator: ArithmeticOperator,
        value_span: Span,
        value: &MiddleExpression,
    ) -> Option<()> {
        self.kind.perform_augmented_assignment_semantic_analysis(
            self.span, ctx, operator, value_span, value,
        )
    }
}
