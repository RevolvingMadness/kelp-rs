use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::low::environment::r#type::TypeId;
use crate::semantic::data_type::SemanticDataType;
use crate::{
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    parsed::typed_path::{ParsedTypedPath, ParsedTypedPathSegment},
    parsed::{
        command::{ParsedCommand, execute::subcommand::r#if::ParsedExecuteIfSubcommand},
        coordinate::ParsedCoordinates,
        data::ParsedDataTarget,
        data_type::ParsedDataType,
        entity_selector::ParsedEntitySelector,
        expression::{
            assignee::{ParsedAssigneeExpression, ParsedAssigneeExpressionKind},
            block::ParsedBlockExpression,
            place::{ParsedPlaceExpression, ParsedPlaceExpressionKind},
        },
        nbt_path::NbtPath,
        pattern::ParsedPattern,
        player_score::ParsedPlayerScore,
        semantic_analysis::{
            FunctionContext, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    runtime_storage::RuntimeStorageType,
    semantic::{
        data::SemanticData,
        expression::{SemanticExpression, SemanticExpressionKind},
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

pub mod assignee;
pub mod block;
pub mod place;

#[derive(Debug, Clone)]
pub enum ParsedExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    InferredInteger(i32),
    Long(i64),
    Float(NotNan<f32>),
    InferredFloat(NotNan<f32>),
    Double(NotNan<f64>),
    String(String),
    Underscore,
    Unit,
    Unary(Span, UnaryOperator, Box<ParsedExpression>),
    Arithmetic(
        Box<ParsedExpression>,
        Span,
        ArithmeticOperator,
        Box<ParsedExpression>,
    ),
    Comparison(
        Box<ParsedExpression>,
        Span,
        ComparisonOperator,
        Box<ParsedExpression>,
    ),
    Logical(
        Box<ParsedExpression>,
        Span,
        LogicalOperator,
        Box<ParsedExpression>,
    ),
    AugmentedAssignment(
        Box<ParsedExpression>,
        Span,
        ArithmeticOperator,
        Box<ParsedExpression>,
    ),
    Assignment(Box<ParsedExpression>, Box<ParsedExpression>),
    List(Vec<ParsedExpression>),
    Compound(HashMap<String, ParsedExpression>),
    PlayerScore(ParsedPlayerScore),
    Data(Box<(ParsedDataTarget, NbtPath)>),
    Condition(bool, Box<ParsedExecuteIfSubcommand>),
    Command(Box<ParsedCommand>),
    Index(Box<ParsedExpression>, Box<ParsedExpression>),
    MethodCall {
        receiver: Box<ParsedExpression>,
        callee: ParsedTypedPathSegment,
        arguments: Vec<ParsedExpression>,
    },
    FieldAccess(Box<ParsedExpression>, Span, String),
    AsCast(Box<ParsedExpression>, ParsedDataType),
    ToCast(Box<ParsedExpression>, RuntimeStorageType),
    Tuple(Vec<ParsedExpression>),
    Path(ParsedTypedPath),
    RegularStruct(ParsedTypedPath, HashMap<(Span, String), ParsedExpression>),
    Call {
        callee: Box<ParsedExpression>,
        arguments: Vec<ParsedExpression>,
    },
    If {
        condition: Box<ParsedExpression>,
        body: Box<ParsedBlockExpression>,
        else_body: Option<Box<ParsedExpression>>,
    },
    Block(ParsedBlockExpression),
    WhileLoop(Box<ParsedExpression>, Box<ParsedBlockExpression>),
    Loop(Box<ParsedBlockExpression>),
    ForLoop(
        bool,
        ParsedPattern,
        Box<ParsedExpression>,
        Box<ParsedBlockExpression>,
    ),
    ResourceLocation(Box<ParsedSupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<ParsedSupportsExpressionSigil<ParsedEntitySelector>>),
    Coordinates(Box<ParsedSupportsExpressionSigil<ParsedCoordinates>>),
    Return(Span, Span, Option<Box<ParsedExpression>>),
    Invalid,
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ParsedExpressionKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> ParsedExpression {
        ParsedExpression { span, kind: self }
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
        if let Self::Arithmetic(left, operator_span, ArithmeticOperator::Multiply, right) = self {
            match right.try_as_f32() {
                Ok(scale) => (Some(scale), left.kind),
                Err(right) => match left.try_as_f32() {
                    Ok(scale) => (Some(scale), right.kind),
                    Err(left) => (
                        None,
                        Self::Arithmetic(
                            Box::new(left),
                            operator_span,
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
    pub const fn into_dummy_expression(self) -> ParsedExpression {
        ParsedExpression {
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedExpression {
    pub span: Span,
    pub kind: ParsedExpressionKind,
}

impl ParsedExpression {
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
    pub fn as_place_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, ParsedPlaceExpression)> {
        let expression = match self.kind {
            ParsedExpressionKind::Path(path) => {
                // let path = path.perform_semantic_analysis(ctx)?;

                // let (id, generic_types, data_type) = ctx.get_visible_value_within_type(path)?;

                // let data_type = data_type?;

                // ParsedPlaceExpressionKind::Value(id.into(), generic_types).with(data_type)

                todo!()
            }
            ParsedExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                ParsedPlaceExpressionKind::Score(score)
                    .with(SemanticDataType::Score(Box::new(SemanticDataType::Integer)))
            }
            ParsedExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx)?;
                let path = path.perform_semantic_analysis(ctx)?;

                ParsedPlaceExpressionKind::Data(Box::new(SemanticData { target, path }))
                    .with(SemanticDataType::Data(Box::new(SemanticDataType::Inferred)))
            }
            ParsedExpressionKind::FieldAccess(target, field_span, field) => {
                let (_, target) = target.as_place_semantic_analysis(ctx)?;

                let field_type = target
                    .data_type
                    .get_field_result_semantic_analysis(ctx, field_span, &field)?;

                ParsedPlaceExpressionKind::FieldAccess(Box::new(target), field).with(field_type)
            }
            ParsedExpressionKind::Index(target, index) => {
                let target = target.as_place_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (_, index) = index?;

                let index_type = target.data_type.get_index_result_semantic_analysis(
                    ctx,
                    target_span,
                    &index.data_type,
                )?;

                ParsedPlaceExpressionKind::Index(Box::new(target), Box::new(index)).with(index_type)
            }
            ParsedExpressionKind::Unary(_, UnaryOperator::Dereference, expression) => {
                let (place_span, place) = expression.as_place_semantic_analysis(ctx)?;

                let dereferenced_type = place
                    .data_type
                    .clone()
                    .get_dereferenced_result_semantic_analysis(ctx, place_span)?;

                ParsedPlaceExpressionKind::Dereference(Box::new(place)).with(dereferenced_type)
            }
            _ => {
                return ctx
                    .add_error(SemanticAnalysisError::ExpressionIsNotAPlace { span: self.span });
            }
        };

        Some((self.span, expression))
    }

    #[must_use]
    pub fn as_assignee_perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, ParsedAssigneeExpression)> {
        let kind = match self.kind {
            ParsedExpressionKind::Tuple(expressions) => {
                let (data_types, expressions) = expressions
                    .into_iter()
                    .map(|expression| {
                        let (_, expression) =
                            expression.as_assignee_perform_semantic_analysis(ctx)?;

                        Some((expression.data_type.clone(), expression))
                    })
                    .collect_option_all::<Vec<_>>()?
                    .into_iter()
                    .unzip();

                ParsedAssigneeExpressionKind::Tuple(expressions)
                    .with(SemanticDataType::Tuple(data_types))
            }
            ParsedExpressionKind::Underscore => {
                ParsedAssigneeExpressionKind::Underscore.with(SemanticDataType::Inferred)
            }
            _ => {
                let (span, place) = self.as_place_semantic_analysis(ctx)?;

                let data_type = place.data_type.clone();

                return Some((
                    span,
                    ParsedAssigneeExpressionKind::Place(place).with(data_type),
                ));
            }
        };

        Some((self.span, kind))
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, SemanticExpression)> {
        let expression = match self.kind {
            ParsedExpressionKind::Invalid => return None,

            ParsedExpressionKind::Unary(operator_span, operator, expression) => match operator {
                UnaryOperator::Negate => {
                    let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                    let Some(negation_result) = expression.data_type.get_negation_result() else {
                        return ctx.add_error(
                            SemanticAnalysisError::CannotApplyUnaryOperatorToType {
                                operator_span,
                                operator,
                                type_span: self.span,
                                data_type: expression.data_type,
                            },
                        );
                    };

                    SemanticExpressionKind::Negate(Box::new(expression)).with(negation_result)
                }
                UnaryOperator::Invert => {
                    let (expression_span, expression) =
                        expression.perform_semantic_analysis(ctx)?;

                    let Some(inverted_result) = expression.data_type.get_inverted_result() else {
                        return ctx.add_error(
                            SemanticAnalysisError::CannotApplyUnaryOperatorToType {
                                operator_span,
                                operator,
                                type_span: expression_span,
                                data_type: expression.data_type,
                            },
                        );
                    };

                    SemanticExpressionKind::Invert(Box::new(expression)).with(inverted_result)
                }
                UnaryOperator::Reference => {
                    let (_, expression) = expression.as_place_semantic_analysis(ctx)?;

                    let data_type =
                        SemanticDataType::Reference(Box::new(expression.data_type.clone()));

                    SemanticExpressionKind::Reference(Box::new(expression)).with(data_type)
                }
                UnaryOperator::Dereference => {
                    let (span, place) = expression.as_place_semantic_analysis(ctx)?;

                    let dereferenced_type = place
                        .data_type
                        .clone()
                        .get_dereferenced_result_semantic_analysis(ctx, span)?;

                    SemanticExpressionKind::Dereference(Box::new(place)).with(dereferenced_type)
                }
            },
            ParsedExpressionKind::Arithmetic(left, operator_span, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (_, left) = left?;
                let (_, right) = right?;

                let Some(result_type) = left.data_type.get_arithmetic_result(&right.data_type)
                else {
                    return ctx.add_error(
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: left.data_type,
                            operator,
                            operator_span,
                            right: right.data_type,
                        },
                    );
                };

                SemanticExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
                    .with(result_type)
            }
            ParsedExpressionKind::Comparison(left, operator_span, operator, right) => {
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
                        SemanticAnalysisError::CannotPerformComparisonOperation {
                            left: left.data_type,
                            operator_span,
                            operator,
                            right: right.data_type,
                        },
                    );
                }

                SemanticExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                    .with(SemanticDataType::Boolean)
            }
            ParsedExpressionKind::Logical(left, _operator_span, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (left_span, left) = left?;
                let (right_span, right) = right?;

                let mut failed = false;

                if left
                    .data_type
                    .assert_equals(ctx, left_span, &SemanticDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if right
                    .data_type
                    .assert_equals(ctx, right_span, &SemanticDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if failed {
                    return None;
                }

                SemanticExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                    .with(SemanticDataType::Boolean)
            }
            ParsedExpressionKind::AugmentedAssignment(target, operator_span, operator, value) => {
                let target = target.as_place_semantic_analysis(ctx);
                let value = value.perform_semantic_analysis(ctx);

                let (_, target) = target?;
                let (_, value) = value?;

                if target
                    .data_type
                    .get_arithmetic_result(&value.data_type)
                    .is_none()
                {
                    return ctx.add_error(
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: target.data_type,
                            operator_span,
                            operator,
                            right: value.data_type,
                        },
                    );
                }

                SemanticExpressionKind::AugmentedAssignment(
                    Box::new(target),
                    operator,
                    Box::new(value),
                )
                .with(SemanticDataType::Unit)
            }
            ParsedExpressionKind::Assignment(target, value) => {
                let target = target.as_assignee_perform_semantic_analysis(ctx);

                let value = value.perform_semantic_analysis(ctx);

                let (_, target) = target?;
                let (value_span, value) = value?;

                target.perform_assignment_semantic_analysis(ctx, value_span, &value.data_type)?;

                SemanticExpressionKind::Assignment(Box::new(target), Box::new(value))
                    .with(SemanticDataType::Unit)
            }
            ParsedExpressionKind::List(expressions) => {
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

                            ctx.add_error_unit(SemanticAnalysisError::MismatchedTypes {
                                span: *expression_span,
                                expected: element_type.clone(),
                                actual: expression.data_type.clone(),
                            });

                            continue;
                        };

                        element_type = reduced_element_type;
                    }

                    if has_error {
                        return None;
                    }

                    element_type
                } else {
                    SemanticDataType::Inferred
                };

                let expressions = expressions
                    .into_iter()
                    .map(|(_, expression)| expression)
                    .collect();

                SemanticExpressionKind::List(expressions)
                    .with(SemanticDataType::List(Box::new(element_type)))
            }
            ParsedExpressionKind::Compound(compound_values) => {
                let compound_values = compound_values
                    .into_iter()
                    .map(|(key, value)| {
                        let (_, value) = value.perform_semantic_analysis(ctx)?;

                        Some((key, value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                let compound_data_types = compound_values
                    .iter()
                    .map(|(key, value)| (key.clone(), value.data_type.clone()))
                    .collect();

                SemanticExpressionKind::Compound(compound_values)
                    .with(SemanticDataType::TypedCompound(compound_data_types))
            }
            ParsedExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::Score(score)
                    .with(SemanticDataType::Score(Box::new(SemanticDataType::Integer)))
            }
            ParsedExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                SemanticExpressionKind::Data(Box::new(SemanticData { target, path }))
                    .with(SemanticDataType::Data(Box::new(SemanticDataType::Inferred)))
            }
            ParsedExpressionKind::Condition(inverted, condition) => {
                let condition = condition.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::Condition(inverted, Box::new(condition))
                    .with(SemanticDataType::Boolean)
            }
            ParsedExpressionKind::Command(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::Command(Box::new(command)).with(SemanticDataType::Integer)
            }
            ParsedExpressionKind::Index(target, index) => {
                let target = target.perform_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (_, index) = index?;

                let index_result = target.data_type.get_index_result_semantic_analysis(
                    ctx,
                    target_span,
                    &index.data_type,
                )?;

                SemanticExpressionKind::Index(Box::new(target), Box::new(index)).with(index_result)
            }
            ParsedExpressionKind::MethodCall {
                receiver,
                callee,
                arguments,
            } => {
                let receiver = receiver.perform_semantic_analysis(ctx);
                let callee_span = callee.name_span;
                let callee = callee.perform_semantic_analysis(ctx);
                let arguments = arguments
                    .into_iter()
                    .map(|expression| expression.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>();

                let (receiver_span, receiver) = receiver?;
                let arguments = arguments?;

                let Some(method_info) = receiver.data_type.get_method(ctx, &callee).ok()? else {
                    return ctx.add_error(SemanticAnalysisError::MethodNotFound {
                        type_span: callee_span,
                        type_: receiver.data_type,
                        method_name: callee.name.clone(),
                    });
                };

                let callee = SemanticExpressionKind::Value(
                    method_info.id.into(),
                    method_info.generic_types.clone(),
                )
                .with(SemanticDataType::Function(
                    method_info.id,
                    method_info.generic_types,
                ));

                let Some(call_info) = callee.data_type.get_call_info(&ctx.semantic_environment)?
                else {
                    return ctx.add_error(SemanticAnalysisError::ExpressionIsNotCallable {
                        callee_span,
                        callee_type: callee.data_type,
                    });
                };

                let mut parameter_types = call_info.parameter_types;

                let parameter_count = parameter_types.len();
                let argument_count = arguments.len() + 1;

                if argument_count != parameter_count {
                    return ctx.add_error(SemanticAnalysisError::MismatchedParameterCount {
                        callee_span,
                        declaration_span: call_info.declaration_span,
                        expected: parameter_count,
                        actual: argument_count,
                    });
                }

                let mut failed = false;
                let mut new_arguments = Vec::with_capacity(argument_count);

                let receiver_param_type = parameter_types.remove(0);
                if receiver.data_type.equals(&receiver_param_type) {
                    new_arguments.push(receiver);
                } else {
                    ctx.add_error_unit(SemanticAnalysisError::MismatchedTypes {
                        span: receiver_span,
                        expected: receiver_param_type,
                        actual: receiver.data_type,
                    });
                    failed = true;
                }

                for (data_type, (argument_span, argument)) in
                    parameter_types.into_iter().zip(arguments)
                {
                    if argument.data_type.equals(&data_type) {
                        if !failed {
                            new_arguments.push(argument);
                        }
                    } else {
                        ctx.add_error_unit(SemanticAnalysisError::MismatchedTypes {
                            span: argument_span,
                            expected: data_type,
                            actual: argument.data_type,
                        });
                        failed = true;
                    }
                }

                if failed {
                    return None;
                }

                if let Some(id) = call_info.id
                    && let FunctionContext::Regular { calls, .. } =
                        ctx.function_contexts.last_mut().unwrap()
                {
                    calls.insert((callee_span, id));
                }

                SemanticExpressionKind::Call(Box::new(callee), new_arguments)
                    .with(call_info.return_type)
            }
            ParsedExpressionKind::FieldAccess(expression, field_span, field) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                let field_result = expression
                    .data_type
                    .get_field_result_semantic_analysis(ctx, field_span, &field)?;

                SemanticExpressionKind::FieldAccess(Box::new(expression), field).with(field_result)
            }
            ParsedExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.perform_semantic_analysis(ctx);
                let data_type = data_type.perform_semantic_analysis(ctx);

                let (expression_span, expression) = expression?;

                if !expression.data_type.can_cast_to(&data_type) {
                    return ctx.add_error(SemanticAnalysisError::CannotCastType {
                        span: expression_span,
                        from: expression.data_type,
                        to: data_type,
                    });
                }

                SemanticExpressionKind::AsCast(Box::new(expression), data_type.clone())
                    .with(data_type)
            }
            ParsedExpressionKind::ToCast(expression, runtime_storage_type) => {
                let (scale, expression) = expression.extract_scale();
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                let data_type = match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if !expression.data_type.can_be_assigned_to_score() {
                            return ctx.add_error(
                                SemanticAnalysisError::TypeIsNotScoreCompatible {
                                    type_span: expression_span,
                                    data_type: expression.data_type,
                                },
                            );
                        }

                        SemanticDataType::Score(Box::new(expression.data_type.clone()))
                    }
                    RuntimeStorageType::Data => {
                        let Some(data_type) = expression
                            .data_type
                            .get_data_type(&ctx.semantic_environment)
                        else {
                            return ctx.add_error(SemanticAnalysisError::TypeIsNotDataCompatible {
                                type_span: expression_span,
                                data_type: expression.data_type,
                            });
                        };

                        SemanticDataType::Data(Box::new(data_type))
                    }
                };

                SemanticExpressionKind::ToCast(scale, Box::new(expression), runtime_storage_type)
                    .with(data_type)
            }
            ParsedExpressionKind::Tuple(expressions) => {
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

                SemanticExpressionKind::Tuple(expressions)
                    .with(SemanticDataType::Tuple(expression_data_types))
            }
            ParsedExpressionKind::RegularStruct(path, supplied_field_values) => {
                let path = path.perform_semantic_analysis(ctx)?;

                let type_id = path.get_type_id(ctx)?;

                let (id, data_type) = ctx.get_struct_id(
                    type_id,
                    &path.generic_spans,
                    &path.generic_types,
                    path.name_span,
                )?;

                let (id, declaration) = ctx.get_regular_struct(id, data_type, path.name_span)?;

                let data_type = SemanticDataType::Struct(id.into(), path.generic_types.clone());

                let declared_generic_ids = declaration.generic_ids.clone();
                let declared_field_types = declaration.field_types.clone();

                let supplied_field_values = supplied_field_values
                    .into_iter()
                    .map(
                        |((supplied_field_span, supplied_field_name), supplied_field_value)| {
                            let Some(declared_field_type) =
                                declared_field_types.get(&supplied_field_name)
                            else {
                                return ctx.add_error(SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: data_type.clone(),
                                    field_span: supplied_field_span,
                                    field: supplied_field_name,
                                });
                            };

                            let declared_field_type = declared_field_type
                                .clone()
                                .substitute_generics(&declared_generic_ids, &path.generic_types);

                            let (supplied_field_value_span, supplied_field_value) =
                                supplied_field_value.perform_semantic_analysis(ctx)?;

                            supplied_field_value.data_type.assert_equals(
                                ctx,
                                supplied_field_value_span,
                                &declared_field_type,
                            )?;

                            Some((supplied_field_name, supplied_field_value))
                        },
                    )
                    .collect_option_all::<HashMap<_, _>>()?;

                let mut has_error = false;

                for declared_field_name in declared_field_types.into_keys() {
                    if !supplied_field_values
                        .keys()
                        .any(|supplied_field_name| *supplied_field_name == declared_field_name)
                    {
                        has_error = true;

                        ctx.add_error_unit(SemanticAnalysisError::MissingField {
                            span: path.name_span,
                            name: declared_field_name.clone(),
                        });
                    }
                }

                if has_error {
                    return None;
                }

                SemanticExpressionKind::RegularStruct(
                    id,
                    path.generic_types.clone(),
                    supplied_field_values,
                )
                .with(data_type)
            }
            ParsedExpressionKind::Call { callee, arguments } => {
                let (callee_span, callee) = callee.perform_semantic_analysis(ctx)?;

                let arguments = arguments
                    .into_iter()
                    .map(|argument| argument.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>()?;

                let Some(call_info) = callee.data_type.get_call_info(&ctx.semantic_environment)?
                else {
                    return ctx.add_error(SemanticAnalysisError::ExpressionIsNotCallable {
                        callee_span,
                        callee_type: callee.data_type,
                    });
                };

                if let Some(id) = call_info.id
                    && let FunctionContext::Regular { calls, .. } =
                        ctx.function_contexts.last_mut().unwrap()
                {
                    calls.insert((callee_span, id));
                }

                let parameter_count = call_info.parameter_types.len();
                let argument_count = arguments.len();

                if argument_count != parameter_count {
                    return ctx.add_error(SemanticAnalysisError::MismatchedParameterCount {
                        callee_span,
                        declaration_span: call_info.declaration_span,
                        expected: parameter_count,
                        actual: argument_count,
                    });
                }

                let mut failed = false;

                let mut new_arguments = Vec::with_capacity(arguments.len());

                for (data_type, (argument_span, argument)) in
                    call_info.parameter_types.into_iter().zip(arguments)
                {
                    if argument.data_type.equals(&data_type) {
                        if !failed {
                            new_arguments.push(argument);
                        }
                    } else {
                        ctx.add_error_unit(SemanticAnalysisError::MismatchedTypes {
                            span: argument_span,
                            expected: data_type,
                            actual: argument.data_type,
                        });

                        failed = true;
                    }
                }

                if failed {
                    return None;
                }

                SemanticExpressionKind::Call(Box::new(callee), new_arguments)
                    .with(call_info.return_type)
            }
            ParsedExpressionKind::If {
                condition,
                body,
                else_body,
            } => {
                let condition = condition.perform_semantic_analysis(ctx);
                let body = body.perform_semantic_analysis(ctx);
                let else_body = else_body.map(|else_body| else_body.perform_semantic_analysis(ctx));

                let (condition_span, condition) = condition?;

                condition.data_type.assert_condition(ctx, condition_span)?;

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
                        &SemanticDataType::Unit,
                    )?;

                    &SemanticDataType::Unit
                };

                let data_type = body.data_type.clone().reduce(else_type).unwrap();

                SemanticExpressionKind::If {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_body: else_body.map(|(_, else_body)| Box::new(else_body)),
                }
                .with(data_type)
            }
            ParsedExpressionKind::Block(expression) => {
                let (_, _, expression) = expression.perform_semantic_analysis(ctx)?;

                expression
            }
            ParsedExpressionKind::WhileLoop(condition, body) => {
                let condition = condition.perform_semantic_analysis(ctx);

                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (condition_span, condition) = condition?;

                condition.data_type.assert_condition(ctx, condition_span)?;

                let (_, _, body) = body?;

                SemanticExpressionKind::WhileLoop(Box::new(condition), Box::new(body))
                    .with(SemanticDataType::Unit)
            }
            ParsedExpressionKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (_, _, body) = body?;

                SemanticExpressionKind::Loop(Box::new(body)).with(SemanticDataType::Unit)
            }
            ParsedExpressionKind::ForLoop(reversed, pattern, iterable, body) => {
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

                SemanticExpressionKind::ForLoop(
                    reversed,
                    Box::new(pattern),
                    Box::new(iterable),
                    Box::new(body),
                )
                .with(SemanticDataType::Unit)
            }
            ParsedExpressionKind::Path(path) => {
                let path = path.perform_semantic_analysis(ctx)?;

                let id = path.get_value_id(ctx)?;

                let declaration = ctx.semantic_environment.get_value(id).clone();

                let data_type = declaration.into_data_type(
                    ctx,
                    id,
                    path.inherited_generic_types,
                    &path.generic_types,
                    path.name_span,
                )?;

                SemanticExpressionKind::Value(id.into(), path.generic_types).with(data_type)
            }
            ParsedExpressionKind::Boolean(value) => {
                SemanticExpressionKind::Boolean(value).with(SemanticDataType::Boolean)
            }
            ParsedExpressionKind::Byte(value) => {
                SemanticExpressionKind::Byte(value).with(SemanticDataType::Byte)
            }
            ParsedExpressionKind::Short(value) => {
                SemanticExpressionKind::Short(value).with(SemanticDataType::Short)
            }
            ParsedExpressionKind::Integer(value) => {
                SemanticExpressionKind::Integer(value).with(SemanticDataType::Integer)
            }
            ParsedExpressionKind::InferredInteger(value) => {
                SemanticExpressionKind::InferredInteger(value)
                    .with(SemanticDataType::InferredInteger)
            }
            ParsedExpressionKind::Long(value) => {
                SemanticExpressionKind::Long(value).with(SemanticDataType::Long)
            }
            ParsedExpressionKind::Float(value) => {
                SemanticExpressionKind::Float(value).with(SemanticDataType::Float)
            }
            ParsedExpressionKind::InferredFloat(value) => {
                SemanticExpressionKind::InferredFloat(value).with(SemanticDataType::InferredFloat)
            }
            ParsedExpressionKind::Double(value) => {
                SemanticExpressionKind::Double(value).with(SemanticDataType::Double)
            }
            ParsedExpressionKind::String(value) => {
                SemanticExpressionKind::String(value).with(SemanticDataType::String)
            }
            ParsedExpressionKind::Underscore => {
                return ctx
                    .add_error(SemanticAnalysisError::UnderscoreExpression { span: self.span });
            }
            ParsedExpressionKind::Unit => SemanticExpressionKind::Unit.with(SemanticDataType::Unit),
            ParsedExpressionKind::ResourceLocation(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::ResourceLocation(Box::new(resource_location))
                    .with(SemanticDataType::ResourceLocation)
            }
            ParsedExpressionKind::EntitySelector(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::EntitySelector(Box::new(selector))
                    .with(SemanticDataType::EntitySelector)
            }
            ParsedExpressionKind::Coordinates(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                SemanticExpressionKind::Coordinates(Box::new(coordinates))
                    .with(SemanticDataType::Coordinates)
            }
            ParsedExpressionKind::Return(keyword_span, expression_span, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        expression
                    }
                    None => SemanticExpressionKind::Unit.with(SemanticDataType::Unit),
                };

                let context = ctx.function_contexts.last().unwrap();

                let return_type = context.return_type();

                if !expression.data_type.equals(return_type) {
                    return ctx.add_error(SemanticAnalysisError::MismatchedTypes {
                        span: expression_span,
                        expected: return_type.clone(),
                        actual: expression.data_type,
                    });
                }

                let context = ctx.function_contexts.last().unwrap();

                if context.is_runtime() == Some(false) {
                    return ctx.add_error(
                        SemanticAnalysisError::CannotUseReturnInCompiletimeFunction {
                            function_declaration_span: context.declaration_span().unwrap(),
                            return_keyword_span: keyword_span,
                        },
                    );
                }

                SemanticExpressionKind::Return(Box::new(expression)).with(SemanticDataType::Never)
            }
        };

        Some((self.span, expression))
    }
}
