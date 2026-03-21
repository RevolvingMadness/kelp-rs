use std::collections::HashMap;

use ordered_float::NotNan;

use crate::{
    high::{
        command::{Command, execute::subcommand::r#if::ExecuteIfSubcommand},
        data::DataTarget,
        data_type::unresolved::UnresolvedDataType,
        nbt_path::NbtPath,
        player_score::PlayerScore,
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    middle::{
        data_type::DataType,
        environment::value::ValueDeclaration,
        expression::{Expression as MiddleExpression, ExpressionKind as MiddleExpressionKind},
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    path::Path,
    place::{PlaceType, PlaceTypeKind},
    runtime_storage_type::RuntimeStorageType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    InferredInteger(i32),
    Long(i64),
    Float(NotNan<f32>),
    InferredFloat(NotNan<f32>),
    Double(NotNan<f64>),
    String(SNBTString),
    Underscore,
    Unit,
    Unary(UnaryOperator, Box<Expression>),
    Arithmetic(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    AugmentedAssignment(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
    Compound(HashMap<SNBTString, Expression>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),
    Condition(bool, Box<ExecuteIfSubcommand>),
    Command(Box<Command>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, SNBTString),
    AsCast(Box<Expression>, UnresolvedDataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Path(Path<UnresolvedDataType>),
    Struct(Path<UnresolvedDataType>, HashMap<SNBTString, Expression>),
    Invalid,
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Expression {
        Expression { span, kind: self }
    }

    #[must_use]
    pub const fn is_f32_compatible(&self) -> bool {
        matches!(
            self,
            Self::Byte(_)
                | Self::Short(_)
                | Self::Integer(_)
                | Self::InferredInteger(_)
                | Self::Long(_)
                | Self::Float(_)
                | Self::InferredFloat(_)
                | Self::Double(_)
        )
    }

    pub fn try_as_f32(self) -> Result<NotNan<f32>, Self> {
        Ok(match self {
            Self::Byte(value) => NotNan::new(f32::from(value)).unwrap(),
            Self::Short(value) => NotNan::new(f32::from(value)).unwrap(),
            Self::Integer(value) | Self::InferredInteger(value) => {
                NotNan::new(value as f32).unwrap()
            }
            Self::Long(value) => NotNan::new(value as f32).unwrap(),
            Self::Float(value) | Self::InferredFloat(value) => value,
            Self::Double(value) => NotNan::new(value.into_inner() as f32).unwrap(),
            _ => return Err(self),
        })
    }

    #[must_use]
    pub fn extract_scale(self) -> (Option<NotNan<f32>>, Self) {
        if let Self::Arithmetic(left, ArithmeticOperator::Multiply, right) = self {
            match right.try_as_f32() {
                Ok(scale) => (Some(scale), left.kind),
                Err(right) => match left.try_as_f32() {
                    Ok(scale) => (Some(scale), right.kind),
                    Err(left) => (
                        None,
                        Self::Arithmetic(
                            Box::new(left),
                            ArithmeticOperator::Multiply,
                            Box::new(right),
                        ),
                    ),
                },
            }
        } else {
            (None, self)
        }
    }

    #[must_use]
    pub const fn into_dummy_expression(self) -> Expression {
        Expression {
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

impl Expression {
    #[must_use]
    pub fn get_place_type(&self, ctx: &mut SemanticAnalysisContext) -> Option<PlaceType> {
        let place_type_kind = match &self.kind {
            ExpressionKind::Tuple(expressions) => PlaceTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| expression.get_place_type(ctx))
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::PlayerScore(_) => PlaceTypeKind::Score(DataType::Integer),
            ExpressionKind::Data(_) => PlaceTypeKind::Data(DataType::SNBT),
            ExpressionKind::Unary(operator, expression) => match operator {
                UnaryOperator::Dereference => {
                    let place_type = expression.get_place_type(ctx)?;

                    PlaceTypeKind::Dereference(Box::new(place_type))
                }
                UnaryOperator::Negate | UnaryOperator::Reference | UnaryOperator::Invert => {
                    return None;
                }
            },
            ExpressionKind::Index(target, _) => {
                let target_place_type = target.get_place_type(ctx)?;

                PlaceTypeKind::Index(Box::new(target_place_type))
            }
            ExpressionKind::FieldAccess(target, field) => {
                let target_place_type = target.get_place_type(ctx)?;

                PlaceTypeKind::FieldAccess(Box::new(target_place_type), field.snbt_string.1.clone())
            }
            ExpressionKind::Invalid
            | ExpressionKind::Boolean(_)
            | ExpressionKind::Byte(_)
            | ExpressionKind::Short(_)
            | ExpressionKind::Integer(_)
            | ExpressionKind::InferredInteger(_)
            | ExpressionKind::Long(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::InferredFloat(_)
            | ExpressionKind::Double(_)
            | ExpressionKind::String(_)
            | ExpressionKind::Arithmetic(_, _, _)
            | ExpressionKind::Comparison(_, _, _)
            | ExpressionKind::Logical(_, _, _)
            | ExpressionKind::AugmentedAssignment(_, _, _)
            | ExpressionKind::Assignment(_, _)
            | ExpressionKind::List(_)
            | ExpressionKind::Compound(_)
            | ExpressionKind::Condition(_, _)
            | ExpressionKind::Command(_)
            | ExpressionKind::AsCast(_, _)
            | ExpressionKind::ToCast(_, _)
            | ExpressionKind::Struct(_, _)
            | ExpressionKind::Unit => return None,
            ExpressionKind::Underscore => PlaceTypeKind::Underscore,
            ExpressionKind::Path(path) => {
                let id = ctx.resolve_value_path(path)?;

                PlaceTypeKind::Value(id)
            }
        };

        Some(place_type_kind.with_span(self.span))
    }

    #[allow(clippy::result_large_err)]
    pub fn try_as_f32(self) -> Result<NotNan<f32>, Self> {
        match self.kind.try_as_f32() {
            Ok(f32) => Ok(f32),
            Err(kind) => Err(Self {
                span: self.span,
                kind,
            }),
        }
    }

    #[must_use]
    pub fn extract_scale(self) -> (Option<NotNan<f32>>, Self) {
        let (scale, kind) = self.kind.extract_scale();

        (
            scale,
            Self {
                span: self.span,
                kind,
            },
        )
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, MiddleExpression)> {
        let expression: MiddleExpression = match self.kind {
            ExpressionKind::Invalid => return None,

            ExpressionKind::Unary(operator, expression) => {
                let (span, expression) = expression.perform_semantic_analysis(ctx)?;

                match operator {
                    UnaryOperator::Negate => {
                        let Some(negation_result) = expression.data_type.get_negation_result()
                        else {
                            return ctx.add_error(
                                self.span,
                                SemanticAnalysisError::CannotNegateType(expression.data_type),
                            );
                        };

                        MiddleExpressionKind::Unary(UnaryOperator::Negate, Box::new(expression))
                            .with(negation_result)
                    }
                    UnaryOperator::Reference => {
                        if !expression.kind.can_be_referenced() {
                            return ctx.add_error(
                                span,
                                SemanticAnalysisError::CannotBeReferenced(expression.data_type),
                            );
                        }

                        let expression_data_type = expression.data_type.clone();

                        MiddleExpressionKind::Unary(UnaryOperator::Reference, Box::new(expression))
                            .with(DataType::Reference(Box::new(expression_data_type)))
                    }
                    UnaryOperator::Dereference => {
                        let Some(dereferenced_result) =
                            expression.data_type.get_dereferenced_result()
                        else {
                            return ctx.add_error(
                                span,
                                SemanticAnalysisError::CannotBeDereferenced(expression.data_type),
                            );
                        };

                        MiddleExpressionKind::Unary(
                            UnaryOperator::Dereference,
                            Box::new(expression),
                        )
                        .with(dereferenced_result)
                    }
                    UnaryOperator::Invert => {
                        let Some(invered_result) = expression.data_type.get_inverted_result()
                        else {
                            return ctx.add_error(
                                self.span,
                                SemanticAnalysisError::CannotInvertType(expression.data_type),
                            );
                        };

                        MiddleExpressionKind::Unary(UnaryOperator::Invert, Box::new(expression))
                            .with(invered_result)
                    }
                }
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                let Some(result_type) = left
                    .data_type
                    .get_arithmetic_result(operator, &right.data_type)
                else {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: left.data_type,
                            operator,
                            right: right.data_type,
                        },
                    );
                };

                if operator == ArithmeticOperator::Swap
                    && (!left.data_type.is_lvalue() || !right.data_type.is_lvalue())
                {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: left.data_type,
                            operator,
                            right: right.data_type,
                        },
                    );
                }

                MiddleExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
                    .with(result_type)
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                if let Some(value) = left.data_type.can_perform_comparison(
                    &ctx.environment,
                    operator,
                    &right.data_type,
                ) && !value
                {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotPerformComparisonOperation {
                            left: left.data_type,
                            operator,
                            right: right.data_type,
                        },
                    );
                }

                MiddleExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                    .with(DataType::Boolean)
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (left_span, left) = left?;
                let (right_span, right) = right?;

                if left.data_type != DataType::Boolean {
                    return ctx.add_error(
                        left_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: DataType::Boolean,
                            actual: left.data_type,
                        },
                    );
                } else if right.data_type != DataType::Boolean {
                    return ctx.add_error(
                        right_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: DataType::Boolean,
                            actual: right.data_type,
                        },
                    );
                }

                MiddleExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                    .with(DataType::Boolean)
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_error(target.span, SemanticAnalysisError::CannotBeAssignedTo);
                };

                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                place.perform_augmented_assignment_semantic_analysis(
                    ctx, operator, value_span, &value,
                )?;

                let (_, target) = target.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::AugmentedAssignment(
                    Box::new(target),
                    operator,
                    Box::new(value),
                )
                .with(DataType::Unit)
            }
            ExpressionKind::Assignment(target, value) => {
                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_error(target.span, SemanticAnalysisError::CannotBeAssignedTo);
                };

                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                place.perform_assignment_semantic_analysis(ctx, value_span, &value)?;

                ctx.is_lhs = true;
                let (_, target) = target.perform_semantic_analysis(ctx)?;
                ctx.is_lhs = false;

                MiddleExpressionKind::Assignment(Box::new(target), Box::new(value))
                    .with(DataType::Unit)
            }
            ExpressionKind::List(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|expression| {
                        let result = expression.perform_semantic_analysis(ctx)?;

                        Some(result)
                    })
                    .collect_option_all::<Vec<_>>()?;

                let element_type = if let Some((_, element_expression)) = expressions.first() {
                    let mut element_type = element_expression.data_type.clone();

                    let mut has_error = false;

                    for (expression_span, expression) in &expressions {
                        let Some(reduced_element_type) =
                            element_type.clone().reduce(&expression.data_type)
                        else {
                            has_error = true;

                            ctx.add_error::<()>(
                                *expression_span,
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: element_type.clone(),
                                    actual: expression.data_type.clone(),
                                },
                            );

                            continue;
                        };

                        element_type = reduced_element_type;
                    }

                    if has_error {
                        return None;
                    }

                    element_type
                } else {
                    DataType::Inferred
                };

                let expressions = expressions
                    .into_iter()
                    .map(|(_, expression)| expression)
                    .collect();

                MiddleExpressionKind::List(expressions).with(DataType::List(Box::new(element_type)))
            }
            ExpressionKind::Compound(compound_values) => {
                let compound_values = compound_values
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                let compound_data_types = compound_values
                    .iter()
                    .map(|(key, value)| (key.clone(), value.data_type.clone()))
                    .collect();

                MiddleExpressionKind::Compound(compound_values)
                    .with(DataType::TypedCompound(compound_data_types))
            }
            ExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::PlayerScore(score)
                    .with(DataType::Score(Box::new(DataType::Integer)))
            }
            ExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                MiddleExpressionKind::Data(Box::new((target, path)))
                    .with(DataType::Data(Box::new(DataType::SNBT)))
            }
            ExpressionKind::Condition(inverted, condition) => {
                let condition = condition.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::Condition(inverted, Box::new(condition))
                    .with(DataType::Boolean)
            }
            ExpressionKind::Command(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::Command(Box::new(command)).with(DataType::Integer)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.perform_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (index_span, index) = index?;

                let Some(index_result) = target.data_type.get_index_result() else {
                    return ctx.add_error(
                        target_span,
                        SemanticAnalysisError::CannotBeIndexed(target.data_type),
                    );
                };

                // TODO: Improve this.
                if target.kind.is_index_out_of_bounds(&index) == Some(true) {
                    return ctx.add_error(index_span, SemanticAnalysisError::IndexOutOfBounds);
                }

                MiddleExpressionKind::Index(Box::new(target), Box::new(index)).with(index_result)
            }
            ExpressionKind::FieldAccess(expression, field) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                if !expression.data_type.has_fields() {
                    return ctx.add_error(
                        field.span,
                        SemanticAnalysisError::TypeDoesntHaveFields(expression.data_type),
                    );
                }

                let (field_span, field) = field.perform_semantic_analysis(ctx);

                let Some(field_result) = expression
                    .data_type
                    .get_field_result(&ctx.environment, &field.1)
                else {
                    return ctx.add_error(
                        field_span,
                        SemanticAnalysisError::TypeDoesntHaveField {
                            data_type: expression.data_type,
                            field: field.1,
                        },
                    );
                };

                MiddleExpressionKind::FieldAccess(Box::new(expression), field).with(field_result)
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.perform_semantic_analysis(ctx);
                let data_type = data_type.resolve_fully(ctx);

                let (_, expression) = expression?;
                let data_type = data_type?;

                if !expression.data_type.can_cast_to(&data_type) {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotCastType {
                            from: expression.data_type,
                            to: data_type,
                        },
                    );
                }

                MiddleExpressionKind::AsCast(Box::new(expression), data_type.clone())
                    .with(data_type)
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let (scale, expression) = expression.extract_scale();
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                let data_type = match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if !expression
                            .data_type
                            .unwrap_all_apply_predicate_all(&ctx.environment, |data_type| {
                                data_type.is_score_compatible()
                            })
                        {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotScoreCompatible(
                                    expression.data_type,
                                ),
                            );
                        }

                        DataType::Score(Box::new(expression.data_type.clone()))
                    }
                    RuntimeStorageType::Data => {
                        DataType::Data(Box::new(expression.data_type.clone()))
                    }
                };

                MiddleExpressionKind::ToCast(scale, Box::new(expression), runtime_storage_type)
                    .with(data_type)
            }
            ExpressionKind::Tuple(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|expression| {
                        expression
                            .perform_semantic_analysis(ctx)
                            .map(|(_, expression)| expression)
                    })
                    .collect_option_all::<Vec<_>>()?;

                let expression_data_types = expressions
                    .iter()
                    .map(|expression| expression.data_type.clone())
                    .collect();

                MiddleExpressionKind::Tuple(expressions)
                    .with(DataType::Tuple(expression_data_types))
            }
            ExpressionKind::Struct(path, field_values) => {
                let path = path.resolve_fully(ctx)?;

                let (id, path_span, last_segment) = ctx.resolve_type_path(path)?;

                let declaration = ctx.get_type(id).clone();

                let generic_types = last_segment.generic_types;

                if let Some(expected_generic_count) = declaration.generic_count() {
                    let actual_generic_count = generic_types.len();

                    if actual_generic_count != expected_generic_count {
                        return ctx.add_invalid_generics(
                            last_segment.span,
                            declaration.name().to_owned(),
                            expected_generic_count,
                            actual_generic_count,
                        );
                    }
                }

                let DataType::Struct(id) =
                    declaration.resolve_fully(ctx, id, generic_types, last_segment.span)?
                else {
                    return ctx.add_error(
                        path_span,
                        SemanticAnalysisError::TypeIsNotStruct(last_segment.name),
                    );
                };

                let declaration = ctx.get_struct_type(id);
                let declared_fields = declaration.field_types.clone();

                let mut has_error = false;

                let given_field_values = field_values
                    .into_iter()
                    .map(|(key, value)| {
                        let Some(field_type) = declared_fields.get(&key.snbt_string.1) else {
                            return ctx.add_error(
                                key.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: DataType::Struct(id),
                                    field: key.snbt_string.1,
                                },
                            );
                        };

                        let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                        if !value.data_type.equals(field_type) {
                            has_error = true;

                            return ctx.add_error(
                                value_span,
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: field_type.clone(),
                                    actual: value.data_type,
                                },
                            );
                        }

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                for declared_field_name in declared_fields.into_keys() {
                    if !given_field_values
                        .keys()
                        .any(|given_field_name| given_field_name.1 == declared_field_name)
                    {
                        has_error = true;

                        ctx.add_error::<()>(
                            path_span,
                            SemanticAnalysisError::MissingField(declared_field_name.clone()),
                        );
                    }
                }

                if has_error {
                    return None;
                }

                MiddleExpressionKind::Struct(id, given_field_values).with(DataType::Struct(id))
            }
            ExpressionKind::Path(path) => {
                let path = path.resolve_fully(ctx)?;

                let id = ctx.resolve_value_path(&path)?;

                let ValueDeclaration::Variable(declaration) = ctx.get_value(id);

                let data_type = declaration.data_type.as_ref()?.clone();

                MiddleExpressionKind::Variable(id).with(data_type)
            }
            ExpressionKind::Boolean(value) => {
                MiddleExpressionKind::Boolean(value).with(DataType::Boolean)
            }
            ExpressionKind::Byte(value) => MiddleExpressionKind::Byte(value).with(DataType::Byte),
            ExpressionKind::Short(value) => {
                MiddleExpressionKind::Short(value).with(DataType::Short)
            }
            ExpressionKind::Integer(value) => {
                MiddleExpressionKind::Integer(value).with(DataType::Integer)
            }
            ExpressionKind::InferredInteger(value) => {
                MiddleExpressionKind::InferredInteger(value).with(DataType::InferredInteger)
            }
            ExpressionKind::Long(value) => MiddleExpressionKind::Long(value).with(DataType::Long),
            ExpressionKind::Float(value) => {
                MiddleExpressionKind::Float(value).with(DataType::Float)
            }
            ExpressionKind::InferredFloat(value) => {
                MiddleExpressionKind::InferredFloat(value).with(DataType::InferredFloat)
            }
            ExpressionKind::Double(value) => {
                MiddleExpressionKind::Double(value).with(DataType::Double)
            }
            ExpressionKind::String(value) => {
                MiddleExpressionKind::String(value.snbt_string).with(DataType::String)
            }
            ExpressionKind::Underscore => {
                if !ctx.is_lhs {
                    return ctx.add_error(self.span, SemanticAnalysisError::UnderscoreExpression);
                }

                MiddleExpressionKind::Underscore.with(DataType::Inferred)
            }
            ExpressionKind::Unit => MiddleExpressionKind::Unit.with(DataType::Unit),
        };

        Some((self.span, expression))
    }
}
