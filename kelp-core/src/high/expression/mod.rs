use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    high::{
        command::{Command, execute::subcommand::r#if::ExecuteIfSubcommand},
        coordinate::Coordinates,
        data::DataTarget,
        data_type::DataType,
        entity_selector::EntitySelector,
        environment::r#type::r#struct::regular::HighStructStructId,
        expression::block::BlockExpression,
        nbt_path::NbtPath,
        pattern::Pattern,
        player_score::PlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    low::{
        data_type::unresolved::UnresolvedDataType,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionKind},
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    path::generic::GenericPath,
    runtime_storage::RuntimeStorageType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

pub mod block;

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
    AsCast(Box<Expression>, DataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Path(GenericPath<DataType>),
    StructStruct(GenericPath<DataType>, HashMap<SNBTString, Expression>),
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    If(
        Box<Expression>,
        Box<BlockExpression>,
        Option<Box<Expression>>,
    ),
    Block(BlockExpression),
    WhileLoop(Box<Expression>, Box<BlockExpression>),
    Loop(Box<BlockExpression>),
    ForLoop(bool, Pattern, Box<Expression>, Box<BlockExpression>),
    ResourceLocation(Box<SupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<SupportsExpressionSigil<EntitySelector>>),
    Coordinates(Box<SupportsExpressionSigil<Coordinates>>),
    Return(Span, Span, Option<Box<Expression>>),
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
            Self::Byte(..)
                | Self::Short(..)
                | Self::Integer(..)
                | Self::InferredInteger(..)
                | Self::Long(..)
                | Self::Float(..)
                | Self::InferredFloat(..)
                | Self::Double(..)
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
    ) -> Option<(Span, UnresolvedExpression)> {
        let expression = match self.kind {
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

                        UnresolvedExpressionKind::Unary(UnaryOperator::Negate, Box::new(expression))
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

                        UnresolvedExpressionKind::Unary(
                            UnaryOperator::Reference,
                            Box::new(expression),
                        )
                        .with(UnresolvedDataType::Reference(Box::new(
                            expression_data_type,
                        )))
                    }
                    UnaryOperator::Dereference => {
                        let dereferenced_result = expression
                            .data_type
                            .clone()
                            .get_dereferenced_result_semantic_analysis(ctx, span)?;

                        UnresolvedExpressionKind::Unary(
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

                        UnresolvedExpressionKind::Unary(UnaryOperator::Invert, Box::new(expression))
                            .with(invered_result)
                    }
                }
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                let Some(result_type) = left.data_type.get_arithmetic_result(&right.data_type)
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

                UnresolvedExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
                    .with(result_type)
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                if !left.data_type.can_perform_comparison(
                    &ctx.environment,
                    operator,
                    &right.data_type,
                ) {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotPerformComparisonOperation {
                            left: left.data_type,
                            operator,
                            right: right.data_type,
                        },
                    );
                }

                UnresolvedExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                    .with(UnresolvedDataType::Boolean)
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (left_span, left) = left?;
                let (right_span, right) = right?;

                let mut failed = false;

                if left
                    .data_type
                    .assert_equals(ctx, left_span, &UnresolvedDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if right
                    .data_type
                    .assert_equals(ctx, right_span, &UnresolvedDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if failed {
                    return None;
                }

                UnresolvedExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                    .with(UnresolvedDataType::Boolean)
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let target = target.perform_semantic_analysis(ctx);
                let value = value.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (value_span, value) = value?;

                if !target.kind.can_be_assigned_to() {
                    return ctx.add_error(target_span, SemanticAnalysisError::CannotBeAssignedTo);
                }

                target.data_type.assert_can_perform_augmented_assignment(
                    ctx,
                    operator,
                    value_span,
                    &value.data_type,
                )?;

                UnresolvedExpressionKind::AugmentedAssignment(
                    Box::new(target),
                    operator,
                    Box::new(value),
                )
                .with(UnresolvedDataType::Unit)
            }
            ExpressionKind::Assignment(target, value) => {
                let value = value.perform_semantic_analysis(ctx);

                ctx.is_lhs += 1;
                let target = target.perform_semantic_analysis(ctx);
                ctx.is_lhs -= 1;

                let (target_span, target) = target?;
                let (value_span, value) = value?;

                if !target.kind.can_be_assigned_to() {
                    return ctx.add_error(target_span, SemanticAnalysisError::CannotBeAssignedTo);
                }

                target.data_type.assert_can_perform_assignment(
                    ctx,
                    value_span,
                    &value.data_type,
                )?;

                UnresolvedExpressionKind::Assignment(Box::new(target), Box::new(value))
                    .with(UnresolvedDataType::Unit)
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

                            ctx.add_error_unit(
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
                    UnresolvedDataType::Inferred
                };

                let expressions = expressions
                    .into_iter()
                    .map(|(_, expression)| expression)
                    .collect();

                UnresolvedExpressionKind::List(expressions)
                    .with(UnresolvedDataType::List(Box::new(element_type)))
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

                UnresolvedExpressionKind::Compound(compound_values)
                    .with(UnresolvedDataType::TypedCompound(compound_data_types))
            }
            ExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Score(score).with(UnresolvedDataType::Score(Box::new(
                    UnresolvedDataType::Integer,
                )))
            }
            ExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                UnresolvedExpressionKind::Data(Box::new((target, path))).with(
                    UnresolvedDataType::Data(Box::new(UnresolvedDataType::Inferred)),
                )
            }
            ExpressionKind::Condition(inverted, condition) => {
                let condition = condition.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Condition(inverted, Box::new(condition))
                    .with(UnresolvedDataType::Boolean)
            }
            ExpressionKind::Command(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Command(Box::new(command))
                    .with(UnresolvedDataType::Integer)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.perform_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (index_span, index) = index?;

                let index_result = target
                    .data_type
                    .get_index_result_semantic_analysis(ctx, target_span)?;

                // TODO: Improve this.
                if target.kind.is_index_out_of_bounds(&index) == Some(true) {
                    return ctx.add_error(index_span, SemanticAnalysisError::IndexOutOfBounds);
                }

                UnresolvedExpressionKind::Index(Box::new(target), Box::new(index))
                    .with(index_result)
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

                let field_result = expression
                    .data_type
                    .get_field_result_semantic_analysis(ctx, field_span, &field.1)?;

                UnresolvedExpressionKind::FieldAccess(Box::new(expression), field)
                    .with(field_result)
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.perform_semantic_analysis(ctx);
                let data_type = data_type.resolve_partially(None, ctx);

                let (_, expression) = expression?;

                if !expression.data_type.can_cast_to(&data_type) {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::CannotCastType {
                            from: expression.data_type,
                            to: data_type,
                        },
                    );
                }

                UnresolvedExpressionKind::AsCast(Box::new(expression), data_type.clone())
                    .with(data_type)
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let (scale, expression) = expression.extract_scale();
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                let data_type = match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if !expression.data_type.can_be_assigned_to_score() {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotScoreCompatible(
                                    expression.data_type,
                                ),
                            );
                        }

                        UnresolvedDataType::Score(Box::new(expression.data_type.clone()))
                    }
                    RuntimeStorageType::Data => {
                        let Some(data_type) =
                            expression.data_type.get_data_type(&ctx.high_environment)
                        else {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotDataCompatible(
                                    expression.data_type,
                                ),
                            );
                        };

                        UnresolvedDataType::Data(Box::new(data_type))
                    }
                };

                UnresolvedExpressionKind::ToCast(scale, Box::new(expression), runtime_storage_type)
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

                UnresolvedExpressionKind::Tuple(expressions)
                    .with(UnresolvedDataType::Tuple(expression_data_types))
            }
            ExpressionKind::StructStruct(path, field_values) => {
                let mut path = path.resolve_partially(None, ctx);

                let id = ctx.get_visible_type_id(&path)?;

                let last_segment = path.segments.pop().unwrap();

                let (_, _, declaration) =
                    ctx.get_visible_struct_struct(last_segment.name_span, &last_segment.name, id)?;

                let id = HighStructStructId(id.0);

                let data_type =
                    UnresolvedDataType::Struct(id.into(), last_segment.generic_types.clone());

                let generic_names = declaration.generic_names.clone();
                let field_types = declaration.field_types.clone();

                let field_values = field_values
                    .into_iter()
                    .map(|(key, value)| {
                        let Some(field_type) = field_types.get(&key.snbt_string.1) else {
                            return ctx.add_error(
                                key.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: data_type.clone(),
                                    field: key.snbt_string.1,
                                },
                            );
                        };

                        let field_type = field_type
                            .clone()
                            .substitute_generics(&generic_names, &last_segment.generic_types);

                        let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                        if !value.data_type.equals(&field_type) {
                            return ctx.add_error(
                                value_span,
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: field_type,
                                    actual: value.data_type,
                                },
                            );
                        }

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                let mut has_error = false;

                for declared_field_name in field_types.into_keys() {
                    if !field_values
                        .keys()
                        .any(|given_field_name| given_field_name.1 == declared_field_name)
                    {
                        has_error = true;

                        ctx.add_error_unit(
                            path.span,
                            SemanticAnalysisError::MissingField(declared_field_name.clone()),
                        );
                    }
                }

                if has_error {
                    return None;
                }

                UnresolvedExpressionKind::StructStruct(id, last_segment.generic_types, field_values)
                    .with(data_type)
            }
            ExpressionKind::Call { callee, arguments } => {
                let (callee_span, callee) = callee.perform_semantic_analysis(ctx)?;

                let arguments = arguments
                    .into_iter()
                    .map(|argument| argument.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>()?;

                let Some(call_info) = callee.data_type.get_call_info(&ctx.high_environment)? else {
                    return ctx
                        .add_error(callee_span, SemanticAnalysisError::ExpressionIsNotCallable);
                };

                let parameter_count = call_info.parameter_types.len();
                let argument_count = arguments.len();

                if argument_count != parameter_count {
                    return ctx.add_error(
                        callee_span,
                        SemanticAnalysisError::MismatchedParameterCount {
                            function_name: call_info.name,
                            expected: parameter_count,
                            actual: argument_count,
                        },
                    );
                }

                let mut failed = false;

                let mut new_arguments = Vec::with_capacity(arguments.len());

                for (data_type, (argument_span, argument)) in call_info
                    .parameter_types
                    .into_iter()
                    .zip(arguments.into_iter())
                {
                    if argument.data_type.equals(&data_type) {
                        if !failed {
                            new_arguments.push(argument);
                        }
                    } else {
                        ctx.add_error_unit(
                            argument_span,
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type,
                                actual: argument.data_type,
                            },
                        );

                        failed = true;
                    }
                }

                if failed {
                    return None;
                }

                UnresolvedExpressionKind::Call(Box::new(callee), new_arguments)
                    .with(call_info.return_type)
            }
            ExpressionKind::If(condition, body, else_body) => {
                let condition = condition.perform_semantic_analysis(ctx);
                let body = body.perform_semantic_analysis(ctx);
                let else_body = else_body.map(|else_body| else_body.perform_semantic_analysis(ctx));

                let (condition_span, condition) = condition?;

                if !condition.data_type.is_condition() {
                    return ctx.add_error(
                        condition_span,
                        SemanticAnalysisError::TypeIsNotCondition(condition.data_type),
                    );
                }

                let (body_span, tail_expression_span, body) = body?;
                let else_body = match else_body {
                    Some(else_body) => Some(else_body?),
                    None => None,
                };

                let else_type = if let Some((else_body_span, else_body)) = &else_body {
                    else_body
                        .data_type
                        .assert_equals(ctx, *else_body_span, &body.data_type)?;

                    &else_body.data_type
                } else {
                    body.data_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(body_span),
                        &UnresolvedDataType::Unit,
                    )?;

                    &UnresolvedDataType::Unit
                };

                let data_type = body.data_type.clone().reduce(else_type).unwrap();

                UnresolvedExpressionKind::If(
                    Box::new(condition),
                    Box::new(body),
                    else_body.map(|(_, else_body)| Box::new(else_body)),
                )
                .with(data_type)
            }
            ExpressionKind::Block(expression) => {
                let (_, _, expression) = expression.perform_semantic_analysis(ctx)?;

                expression
            }
            ExpressionKind::WhileLoop(condition, body) => {
                let condition = condition.perform_semantic_analysis(ctx);

                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (condition_span, condition) = condition?;

                condition.data_type.assert_condition(ctx, condition_span)?;

                let (_, _, body) = body?;

                UnresolvedExpressionKind::WhileLoop(Box::new(condition), Box::new(body))
                    .with(UnresolvedDataType::Unit)
            }
            ExpressionKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (_, _, body) = body?;

                UnresolvedExpressionKind::Loop(Box::new(body)).with(UnresolvedDataType::Unit)
            }
            ExpressionKind::ForLoop(reversed, pattern, iterable, body) => {
                let (expression_span, iterable) = iterable.perform_semantic_analysis(ctx)?;

                let Some(iterable_type) = iterable
                    .data_type
                    .clone()
                    .get_iterable_type_semantic_analysis(ctx, expression_span)
                else {
                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                ctx.enter_scope();

                let Some(pattern) = pattern.perform_semantic_analysis(ctx, &iterable_type) else {
                    ctx.exit_scope();

                    return None;
                };

                ctx.loop_depth += 1;
                let Some((_, _, body)) = body.perform_semantic_analysis(ctx) else {
                    ctx.loop_depth -= 1;

                    ctx.exit_scope();

                    return None;
                };
                ctx.loop_depth -= 1;
                ctx.exit_scope();

                // TODO: Reorder semantic analysis

                UnresolvedExpressionKind::ForLoop(
                    reversed,
                    Box::new(pattern),
                    Box::new(iterable),
                    Box::new(body),
                )
                .with(UnresolvedDataType::Unit)
            }
            ExpressionKind::Path(path) => {
                let mut path = path.resolve_partially(None, ctx);

                let id = ctx.get_visible_value_id(&path)?;

                let value_declaration = ctx.get_value(id).clone();

                let last_segment = path.segments.pop().unwrap();

                let (id, data_type) = value_declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_types.clone(),
                    last_segment.name_span,
                )?;

                UnresolvedExpressionKind::Value(id, last_segment.generic_types).with(data_type)
            }
            ExpressionKind::Boolean(value) => {
                UnresolvedExpressionKind::Boolean(value).with(UnresolvedDataType::Boolean)
            }
            ExpressionKind::Byte(value) => {
                UnresolvedExpressionKind::Byte(value).with(UnresolvedDataType::Byte)
            }
            ExpressionKind::Short(value) => {
                UnresolvedExpressionKind::Short(value).with(UnresolvedDataType::Short)
            }
            ExpressionKind::Integer(value) => {
                UnresolvedExpressionKind::Integer(value).with(UnresolvedDataType::Integer)
            }
            ExpressionKind::InferredInteger(value) => {
                UnresolvedExpressionKind::InferredInteger(value)
                    .with(UnresolvedDataType::InferredInteger)
            }
            ExpressionKind::Long(value) => {
                UnresolvedExpressionKind::Long(value).with(UnresolvedDataType::Long)
            }
            ExpressionKind::Float(value) => {
                UnresolvedExpressionKind::Float(value).with(UnresolvedDataType::Float)
            }
            ExpressionKind::InferredFloat(value) => UnresolvedExpressionKind::InferredFloat(value)
                .with(UnresolvedDataType::InferredFloat),
            ExpressionKind::Double(value) => {
                UnresolvedExpressionKind::Double(value).with(UnresolvedDataType::Double)
            }
            ExpressionKind::String(value) => {
                UnresolvedExpressionKind::String(value.snbt_string).with(UnresolvedDataType::String)
            }
            ExpressionKind::Underscore => {
                if ctx.is_lhs == 0 {
                    return ctx.add_error(self.span, SemanticAnalysisError::UnderscoreExpression);
                }

                UnresolvedExpressionKind::Underscore.with(UnresolvedDataType::Inferred)
            }
            ExpressionKind::Unit => UnresolvedExpressionKind::Unit.with(UnresolvedDataType::Unit),
            ExpressionKind::ResourceLocation(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::ResourceLocation(Box::new(resource_location))
                    .with(UnresolvedDataType::ResourceLocation)
            }
            ExpressionKind::EntitySelector(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::EntitySelector(Box::new(selector))
                    .with(UnresolvedDataType::EntitySelector)
            }
            ExpressionKind::Coordinates(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Coordinates(Box::new(coordinates))
                    .with(UnresolvedDataType::Coordinates)
            }
            ExpressionKind::Return(keyword_span, expression_span, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        expression
                    }
                    None => UnresolvedExpressionKind::Unit.with(UnresolvedDataType::Unit),
                };

                if let Some(return_type) = ctx.function_return_types.last()
                    && !expression.data_type.equals(return_type)
                {
                    return ctx.add_error(
                        expression_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: return_type.clone(),
                            actual: expression.data_type,
                        },
                    );
                }

                let return_type = ctx.function_return_types.last().unwrap();

                if return_type.is_compiletime()? {
                    return ctx.add_error(
                        keyword_span,
                        SemanticAnalysisError::CannotUseReturnWithCompiletimeResult,
                    );
                }

                UnresolvedExpressionKind::Return(Box::new(expression))
                    .with(UnresolvedDataType::Never)
            }
        };

        Some((self.span, expression))
    }
}
