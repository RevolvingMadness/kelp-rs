use std::collections::BTreeMap;

use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    high::{
        command::{Command, execute::subcommand::r#if::ExecuteIfSubcommand},
        data::DataTarget,
        data_type::DataType,
        nbt_path::NbtPath,
        player_score::PlayerScore,
        snbt_string::SNBTString,
    },
    middle::{
        data_type::DataTypeKind,
        expression::{Expression as MiddleExpression, ExpressionKind as MiddleExpressionKind},
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::{PlaceType, PlaceTypeKind},
    runtime_storage_type::RuntimeStorageType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
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
    Compound(BTreeMap<SNBTString, Expression>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),
    Condition(bool, Box<ExecuteIfSubcommand>),
    Command(Box<Command>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, SNBTString),
    AsCast(Box<Expression>, DataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Variable(String),
    Struct(
        Span,
        String,
        Vec<DataType>,
        BTreeMap<SNBTString, Expression>,
    ),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

impl Expression {
    #[must_use]
    pub fn get_dereferenced_place_type(&self, ctx: &SemanticAnalysisContext) -> Option<PlaceType> {
        let place_type_kind = match &self.kind {
            ExpressionKind::PlayerScore(_) => PlaceTypeKind::Score(DataTypeKind::Integer),
            ExpressionKind::Data(_) => PlaceTypeKind::Data(DataTypeKind::SNBT),
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
            | ExpressionKind::Underscore
            | ExpressionKind::Unit
            | ExpressionKind::Arithmetic(_, _, _)
            | ExpressionKind::Comparison(_, _, _)
            | ExpressionKind::Logical(_, _, _)
            | ExpressionKind::AugmentedAssignment(_, _, _)
            | ExpressionKind::Assignment(_, _)
            | ExpressionKind::List(_)
            | ExpressionKind::Compound(_)
            | ExpressionKind::Condition(_, _)
            | ExpressionKind::Command(_)
            | ExpressionKind::Index(_, _)
            | ExpressionKind::FieldAccess(_, _)
            | ExpressionKind::AsCast(_, _)
            | ExpressionKind::ToCast(_, _)
            | ExpressionKind::Tuple(_)
            | ExpressionKind::Struct(_, _, _, _) => return None,
            ExpressionKind::Unary(operator, expression) => match operator {
                UnaryOperator::Reference => return expression.get_place_type(ctx),
                UnaryOperator::Dereference | UnaryOperator::Negate | UnaryOperator::Invert => {
                    return None;
                }
            },
            ExpressionKind::Variable(name) => {
                ctx.get_variable(name)??.as_dereferenced_place_type().ok()?
            }
        };

        Some(place_type_kind.with_span(self.span))
    }

    #[must_use]
    pub fn get_place_type(&self, ctx: &SemanticAnalysisContext) -> Option<PlaceType> {
        let place_type_kind = match &self.kind {
            ExpressionKind::Tuple(expressions) => PlaceTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| expression.get_place_type(ctx))
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::PlayerScore(_) => PlaceTypeKind::Score(DataTypeKind::Integer),
            ExpressionKind::Data(_) => PlaceTypeKind::Data(DataTypeKind::SNBT),
            ExpressionKind::Unary(operator, expression) => match operator {
                UnaryOperator::Dereference => return expression.get_dereferenced_place_type(ctx),
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
            | ExpressionKind::Struct(_, _, _, _)
            | ExpressionKind::Unit => return None,
            ExpressionKind::Underscore => PlaceTypeKind::Underscore,
            ExpressionKind::Variable(name) => {
                let variable_type = ctx.get_variable(name)??;

                PlaceTypeKind::Variable(variable_type)
            }
        };

        Some(place_type_kind.with_span(self.span))
    }

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
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: self.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotNegateType(expression.data_type),
                                ),
                            });
                        };

                        MiddleExpressionKind::Unary(UnaryOperator::Negate, Box::new(expression))
                            .with(negation_result)
                    }
                    UnaryOperator::Reference => {
                        if !expression.kind.can_be_referenced() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeReferenced(expression.data_type),
                                ),
                            });
                        }

                        let expression_data_type = expression.data_type.clone();

                        MiddleExpressionKind::Unary(UnaryOperator::Reference, Box::new(expression))
                            .with(DataTypeKind::Reference(Box::new(expression_data_type)))
                    }
                    UnaryOperator::Dereference => {
                        let Some(dereferenced_result) =
                            expression.data_type.get_dereferenced_result()
                        else {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeDereferenced(
                                        expression.data_type,
                                    ),
                                ),
                            });
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
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: self.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotInvertType(expression.data_type),
                                ),
                            });
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

                let Some(result_type) =
                    left.data_type
                        .get_arithmetic_result(ctx, operator, &right.data_type)
                else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformArithmeticOperation {
                                left: left.data_type,
                                operator,
                                right: right.data_type,
                            },
                        ),
                    });
                };

                if operator == ArithmeticOperator::Swap
                    && (!left.data_type.is_lvalue() || !right.data_type.is_lvalue())
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformArithmeticOperation {
                                left: left.data_type,
                                operator,
                                right: right.data_type,
                            },
                        ),
                    });
                }

                MiddleExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
                    .with(result_type)
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                if let Some(value) =
                    left.data_type
                        .can_perform_comparison(ctx, operator, &right.data_type)
                    && !value
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformComparisonOperation {
                                left: left.data_type,
                                operator,
                                right: right.data_type,
                            },
                        ),
                    });
                }

                MiddleExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                    .with(DataTypeKind::Boolean)
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (left_span, left) = left?;
                let (right_span, right) = right?;

                if left.data_type != DataTypeKind::Boolean {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: left_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: DataTypeKind::Boolean,
                                actual: left.data_type,
                            },
                        ),
                    });
                } else if right.data_type != DataTypeKind::Boolean {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: right_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: DataTypeKind::Boolean,
                                actual: right.data_type,
                            },
                        ),
                    });
                }

                MiddleExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                    .with(DataTypeKind::Boolean)
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
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
                .with(DataTypeKind::Unit)
            }
            ExpressionKind::Assignment(target, value) => {
                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                place.perform_assignment_semantic_analysis(ctx, value_span, &value)?;

                ctx.is_lhs = true;
                let (_, target) = target.perform_semantic_analysis(ctx)?;
                ctx.is_lhs = false;

                MiddleExpressionKind::Assignment(Box::new(target), Box::new(value))
                    .with(DataTypeKind::Unit)
            }
            ExpressionKind::List(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|expression| {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    })
                    .collect_option_all::<Vec<_>>()?;

                MiddleExpressionKind::List(expressions)
                    .with(DataTypeKind::List(Box::new(DataTypeKind::Inferred)))
            }
            ExpressionKind::Compound(compound_values) => {
                let compound_values = compound_values
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all::<BTreeMap<_, _>>()?;

                let compound_data_types = compound_values
                    .iter()
                    .map(|(key, value)| (key.clone(), value.data_type.clone()))
                    .collect();

                MiddleExpressionKind::Compound(compound_values)
                    .with(DataTypeKind::TypedCompound(compound_data_types))
            }
            ExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::PlayerScore(score)
                    .with(DataTypeKind::Score(Box::new(DataTypeKind::Integer)))
            }
            ExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                MiddleExpressionKind::Data(Box::new((target, path)))
                    .with(DataTypeKind::Data(Box::new(DataTypeKind::SNBT)))
            }
            ExpressionKind::Condition(inverted, condition) => {
                let condition = condition.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::Condition(inverted, Box::new(condition))
                    .with(DataTypeKind::Boolean)
            }
            ExpressionKind::Command(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleExpressionKind::Command(Box::new(command)).with(DataTypeKind::Integer)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.perform_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (index_span, index) = index?;

                let Some(index_result) = target.data_type.get_index_result() else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeIndexed(target.data_type),
                        ),
                    });
                };

                // TODO: Improve this.
                if target.kind.is_index_out_of_bounds(&index) == Some(true) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: index_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::IndexOutOfBounds,
                        ),
                    });
                }

                MiddleExpressionKind::Index(Box::new(target), Box::new(index)).with(index_result)
            }
            ExpressionKind::FieldAccess(expression, field) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                if !expression.data_type.has_fields() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: field.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeDoesntHaveFields(expression.data_type),
                        ),
                    });
                }

                let (field_span, field) = field.perform_semantic_analysis(ctx);

                let Some(field_result) = expression.data_type.get_field_result(ctx, &field.1)
                else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: field_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeDoesntHaveField {
                                data_type: expression.data_type,
                                field: field.1,
                            },
                        ),
                    });
                };

                MiddleExpressionKind::FieldAccess(Box::new(expression), field).with(field_result)
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.perform_semantic_analysis(ctx);
                let data_type = data_type.perform_semantic_analysis(None, ctx);

                let (_, expression) = expression?;
                let data_type = data_type?;

                if !expression.data_type.can_cast_to(&data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotCastType {
                                from: expression.data_type,
                                to: data_type,
                            },
                        ),
                    });
                }

                MiddleExpressionKind::AsCast(Box::new(expression), data_type.clone())
                    .with(data_type)
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let (scale, expression) = expression.extract_scale();
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                let data_type = match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if let Some(value) = expression.data_type.is_score_compatible(ctx)
                            && !value
                        {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression_span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::TypeIsNotScoreCompatible(
                                        expression.data_type,
                                    ),
                                ),
                            });
                        }

                        DataTypeKind::Score(Box::new(DataTypeKind::Integer))
                    }
                    RuntimeStorageType::Data => {
                        DataTypeKind::Data(Box::new(expression.data_type.clone()))
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
                    .with(DataTypeKind::Tuple(expression_data_types))
            }
            ExpressionKind::Struct(name_span, name, generic_types, field_values) => {
                let declaration = ctx.get_data_type(&name);

                let declaration = match declaration {
                    None => {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: name_span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnknownType(name),
                            ),
                        });
                    }
                    Some(Some(declaration)) => declaration,
                    Some(None) => return None,
                };

                let generic_types = generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.perform_semantic_analysis(None, ctx))
                    .collect_option_all::<Vec<_>>()?;

                let defined_fields = declaration.get_struct_fields(ctx, &generic_types);

                if !declaration.resolve_is_struct(ctx, generic_types.clone())? {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: name_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotStruct(name),
                        ),
                    });
                }

                let defined_fields = defined_fields?;

                let mut has_error = false;

                let field_values = field_values
                    .into_iter()
                    .map(|(key, value)| {
                        if !defined_fields.contains_key(&key.snbt_string.1) {
                            has_error = true;

                            ctx.add_info::<()>(SemanticAnalysisInfo {
                                span: key.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnexpectedField(key.snbt_string.1),
                                ),
                            });

                            return None;
                        }

                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all::<BTreeMap<_, _>>()?;

                for defined_field in defined_fields.keys() {
                    if !field_values.keys().any(|field| field.1 == *defined_field) {
                        has_error = true;

                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: self.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::MissingField(defined_field.clone()),
                            ),
                        });
                    }
                }

                if has_error {
                    return None;
                }

                MiddleExpressionKind::Struct(name.clone(), generic_types.clone(), field_values)
                    .with(DataTypeKind::Struct(name, generic_types))
            }
            ExpressionKind::Variable(name) => {
                let Some(variable_data_type) = ctx.get_variable(&name) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UndeclaredVariable(name),
                        ),
                    });
                };

                let variable_data_type = variable_data_type?;

                MiddleExpressionKind::Variable(name).with(variable_data_type)
            }
            ExpressionKind::Boolean(value) => {
                MiddleExpressionKind::Boolean(value).with(DataTypeKind::Boolean)
            }
            ExpressionKind::Byte(value) => {
                MiddleExpressionKind::Byte(value).with(DataTypeKind::Byte)
            }
            ExpressionKind::Short(value) => {
                MiddleExpressionKind::Short(value).with(DataTypeKind::Short)
            }
            ExpressionKind::Integer(value) => {
                MiddleExpressionKind::Integer(value).with(DataTypeKind::Integer)
            }
            ExpressionKind::InferredInteger(value) => {
                MiddleExpressionKind::InferredInteger(value).with(DataTypeKind::InferredInteger)
            }
            ExpressionKind::Long(value) => {
                MiddleExpressionKind::Long(value).with(DataTypeKind::Long)
            }
            ExpressionKind::Float(value) => {
                MiddleExpressionKind::Float(value).with(DataTypeKind::Float)
            }
            ExpressionKind::InferredFloat(value) => {
                MiddleExpressionKind::InferredFloat(value).with(DataTypeKind::InferredFloat)
            }
            ExpressionKind::Double(value) => {
                MiddleExpressionKind::Double(value).with(DataTypeKind::Double)
            }
            ExpressionKind::String(value) => {
                MiddleExpressionKind::String(value.snbt_string).with(DataTypeKind::String)
            }
            ExpressionKind::Underscore => {
                if !ctx.is_lhs {
                    return ctx
                        .add_error_ret(self.span, SemanticAnalysisError::UnderscoreExpression);
                }

                MiddleExpressionKind::Underscore.with(DataTypeKind::Inferred)
            }
            ExpressionKind::Unit => MiddleExpressionKind::Unit.with(DataTypeKind::Unit),
        };

        Some((self.span, expression))
    }
}
