use std::collections::HashMap;

use la_arena::Idx;
use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    parsed::arena::{ParsedAstArena, Spanned},
    parsed::{
        command::{
            Command,
            execute::subcommand::r#if::ParsedExecuteIfSubcommand::{self},
        },
        coordinate::Coordinates,
        data::DataTarget,
        data_type::ParsedDataType,
        entity_selector::EntitySelector,
        expression::{
            assignee::{ParsedAssigneeExpression, ParsedAssigneeExpressionId},
            block::BlockExpression,
            place::{ParsedPlaceExpression, ParsedPlaceExpressionId},
        },
        nbt_path::ParsedNbtPath,
        pattern::Pattern,
        player_score::PlayerScore,
        semantic_analysis::{
            FunctionContext, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    path::generic::{GenericPath, GenericPathSegment},
    runtime_storage::RuntimeStorageType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::{
        data::TypedData,
        data_type::SemanticDataType,
        environment::r#type::r#struct::regular::HighRegularStructId,
        expression::{TypedExpression, TypedExpressionId},
    },
};

pub mod assignee;
pub mod block;
pub mod place;

pub type ParsedExpressionId = Idx<Spanned<ParsedExpression>>;

#[derive(Debug, Clone)]
pub enum ParsedExpression {
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
    Unary(UnaryOperator, ParsedExpressionId),
    Arithmetic(ParsedExpressionId, ArithmeticOperator, ParsedExpressionId),
    Comparison(ParsedExpressionId, ComparisonOperator, ParsedExpressionId),
    Logical(ParsedExpressionId, LogicalOperator, ParsedExpressionId),
    AugmentedAssignment(
        ParsedExpressionId,
        Span,
        ArithmeticOperator,
        ParsedExpressionId,
    ),
    Assignment(ParsedExpressionId, ParsedExpressionId),
    List(Vec<ParsedExpressionId>),
    Compound(HashMap<String, ParsedExpressionId>),
    PlayerScore(PlayerScore),
    Data(Box<(DataTarget, ParsedNbtPath)>),
    Condition(bool, Box<ParsedExecuteIfSubcommand>),
    Command(Box<Command>),
    Index(ParsedExpressionId, ParsedExpressionId),
    MethodCall {
        receiver: ParsedExpressionId,
        callee: GenericPathSegment<ParsedDataType>,
        arguments: Vec<ParsedExpressionId>,
    },
    FieldAccess(ParsedExpressionId, Span, String),
    AsCast(ParsedExpressionId, ParsedDataType),
    ToCast(ParsedExpressionId, RuntimeStorageType),
    Tuple(Vec<ParsedExpressionId>),
    Path(GenericPath<ParsedDataType>),
    RegularStruct(
        GenericPath<ParsedDataType>,
        HashMap<(Span, String), ParsedExpressionId>,
    ),
    Call {
        callee: ParsedExpressionId,
        arguments: Vec<ParsedExpressionId>,
    },
    If {
        condition: ParsedExpressionId,
        body: Box<BlockExpression>,
        else_body: Option<ParsedExpressionId>,
    },
    Block(BlockExpression),
    WhileLoop(ParsedExpressionId, Box<BlockExpression>),
    Loop(Box<BlockExpression>),
    ForLoop(bool, Idx<Pattern>, ParsedExpressionId, Box<BlockExpression>),
    ResourceLocation(Box<ParsedSupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<ParsedSupportsExpressionSigil<EntitySelector>>),
    Coordinates(Box<ParsedSupportsExpressionSigil<Coordinates>>),
    Return(Span, Span, Option<ParsedExpressionId>),
    Invalid,
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ParsedExpression {
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

    #[must_use]
    pub fn try_as_f32(id: ParsedExpressionId, arena: &ParsedAstArena) -> Option<NotNan<f32>> {
        Some(match arena.get_expression_value(id) {
            Self::Byte(value) => NotNan::new(f32::from(*value)).unwrap(),
            Self::Short(value) => NotNan::new(f32::from(*value)).unwrap(),
            Self::Integer(value) | Self::InferredInteger(value) => {
                NotNan::new(*value as f32).unwrap()
            }
            Self::Long(value) => NotNan::new(*value as f32).unwrap(),
            Self::Float(value) | Self::InferredFloat(value) => *value,
            Self::Double(value) => NotNan::new(value.into_inner() as f32).unwrap(),
            _ => return None,
        })
    }

    #[must_use]
    pub fn extract_scale(
        id: ParsedExpressionId,
        arena: &ParsedAstArena,
    ) -> (Option<NotNan<f32>>, ParsedExpressionId) {
        if let Self::Arithmetic(left, ArithmeticOperator::Multiply, right) =
            arena.get_expression_value(id)
        {
            match Self::try_as_f32(*right, arena) {
                Some(scale) => (Some(scale), *left),
                None => match Self::try_as_f32(*left, arena) {
                    Some(scale) => (Some(scale), *right),
                    None => (None, id),
                },
            }
        } else {
            (None, id)
        }
    }
}

impl ParsedExpression {
    #[must_use]
    pub fn as_place_semantic_analysis(
        id: ParsedExpressionId,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ParsedPlaceExpressionId> {
        let id = match parsed_arena.get_expression_value(id) {
            Self::Path(path) => {
                let mut path = path.clone().perform_semantic_analysis(ctx);

                let id = ctx.get_visible_value_id(&path)?;

                let value_declaration = ctx.semantic_environment.get_value(id).clone();

                let last_segment = path.segments.pop().unwrap();

                let (id, data_type) = value_declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_types.clone(),
                    last_segment.name_span,
                )?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::Value(id, last_segment.generic_types),
                    data_type,
                )
            }
            Self::PlayerScore(score) => {
                let score =
                    score
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::Score(score),
                    SemanticDataType::Score(Box::new(SemanticDataType::Integer)),
                )
            }
            Self::Data(target_path) => {
                let (target, path) = &**target_path;

                let target =
                    target
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;
                let path =
                    path.clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::Data(Box::new(TypedData { target, path })),
                    SemanticDataType::Data(Box::new(SemanticDataType::Inferred)),
                )
            }
            Self::FieldAccess(target, field_span, field) => {
                let target =
                    Self::as_place_semantic_analysis(*target, parsed_arena, typed_arena, ctx)?;

                let target_type = typed_arena.get_place_expression_type(target);

                let field_type =
                    target_type.get_field_result_semantic_analysis(ctx, *field_span, field)?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::FieldAccess(target, field.clone()),
                    field_type,
                )
            }
            Self::Index(target, index) => {
                let target_span = parsed_arena.get_expression_span(*target);

                let target =
                    Self::as_place_semantic_analysis(*target, parsed_arena, typed_arena, ctx);

                let index = Self::perform_semantic_analysis(*index, parsed_arena, typed_arena, ctx);

                let target = target?;
                let index = index?;

                let target_type = typed_arena.get_place_expression_type(target);
                let index_type = typed_arena.get_expression_type(index);

                let index_type =
                    target_type.get_index_result_semantic_analysis(ctx, target_span, index_type)?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::Index(target, index),
                    index_type,
                )
            }
            Self::Unary(UnaryOperator::Dereference, expression) => {
                let place =
                    Self::as_place_semantic_analysis(*expression, parsed_arena, typed_arena, ctx)?;

                let place_type = typed_arena.get_place_expression_type(place);
                let place_span = parsed_arena.get_expression_span(*expression);

                let dereferenced_type = place_type
                    .clone()
                    .get_dereferenced_result_semantic_analysis(ctx, place_span)?;

                typed_arena.allocate_place_expression(
                    ParsedPlaceExpression::Dereference(place),
                    dereferenced_type,
                )
            }
            _ => {
                let span = parsed_arena.get_expression_span(id);

                return ctx.add_error(span, SemanticAnalysisError::ExpressionIsNotAPlace);
            }
        };

        Some(id)
    }

    #[must_use]
    pub fn as_assignee_perform_semantic_analysis(
        id: ParsedExpressionId,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ParsedAssigneeExpressionId> {
        Some(match parsed_arena.get_expression_value(id) {
            Self::Tuple(expressions) => {
                let (data_types, expressions) = expressions
                    .iter()
                    .copied()
                    .map(|expression| {
                        let expression = Self::as_assignee_perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        let expression_type = typed_arena.get_assignee_expression_type(expression);

                        Some((expression_type.clone(), expression))
                    })
                    .collect_option_all::<Vec<_>>()?
                    .into_iter()
                    .unzip();

                typed_arena.allocate_assignee_expression(
                    ParsedAssigneeExpression::Tuple(expressions),
                    SemanticDataType::Tuple(data_types),
                )
            }
            Self::Underscore => typed_arena.allocate_assignee_expression(
                ParsedAssigneeExpression::Underscore,
                SemanticDataType::Inferred,
            ),
            _ => {
                let place = Self::as_place_semantic_analysis(id, parsed_arena, typed_arena, ctx)?;

                let place_type = typed_arena.get_place_expression_type(place);

                typed_arena.allocate_assignee_expression(
                    ParsedAssigneeExpression::Place(place),
                    place_type.clone(),
                )
            }
        })
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        id: ParsedExpressionId,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedExpressionId> {
        Some(match parsed_arena.get_expression_value(id) {
            Self::Invalid => return None,

            Self::Unary(operator, expression) => match operator {
                UnaryOperator::Negate => {
                    let expression = Self::perform_semantic_analysis(
                        *expression,
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?;

                    let expression_type = typed_arena.get_expression_type(expression);

                    let Some(negation_result) = expression_type.get_negation_result() else {
                        let span = parsed_arena.get_expression_span(id);

                        return ctx.add_error(
                            span,
                            SemanticAnalysisError::CannotNegateType(expression_type.clone()),
                        );
                    };

                    typed_arena
                        .allocate_expression(TypedExpression::Negate(expression), negation_result)
                }
                UnaryOperator::Invert => {
                    let expression_span = parsed_arena.get_expression_span(*expression);

                    let expression = Self::perform_semantic_analysis(
                        *expression,
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?;

                    let expression_type = typed_arena.get_expression_type(expression);

                    let Some(inverted_result) = expression_type.get_inverted_result() else {
                        return ctx.add_error(
                            expression_span,
                            SemanticAnalysisError::CannotInvertType(expression_type.clone()),
                        );
                    };

                    typed_arena
                        .allocate_expression(TypedExpression::Invert(expression), inverted_result)
                }
                UnaryOperator::Reference => {
                    let place = Self::as_place_semantic_analysis(
                        *expression,
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?;

                    let place_type = typed_arena.get_place_expression_type(place);

                    typed_arena.allocate_expression(
                        TypedExpression::Reference(place),
                        SemanticDataType::Reference(Box::new(place_type.clone())),
                    )
                }
                UnaryOperator::Dereference => {
                    let expression_span = parsed_arena.get_expression_span(*expression);

                    let place = Self::as_place_semantic_analysis(
                        *expression,
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?;

                    let place_type = typed_arena.get_place_expression_type(place);

                    let dereferenced_type = place_type
                        .clone()
                        .get_dereferenced_result_semantic_analysis(ctx, expression_span)?;

                    typed_arena
                        .allocate_expression(TypedExpression::Dereference(place), dereferenced_type)
                }
            },
            Self::Arithmetic(left, operator, right) => {
                let operator = *operator;

                let left = Self::perform_semantic_analysis(*left, parsed_arena, typed_arena, ctx);
                let right = Self::perform_semantic_analysis(*right, parsed_arena, typed_arena, ctx);

                let left = left?;
                let right = right?;

                let left_type = typed_arena.get_expression_type(left);
                let right_type = typed_arena.get_expression_type(right);

                let Some(result_type) = left_type.get_arithmetic_result(right_type) else {
                    let span = parsed_arena.get_expression_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: left_type.clone(),
                            operator,
                            right: right_type.clone(),
                        },
                    );
                };

                typed_arena.allocate_expression(
                    TypedExpression::Arithmetic(left, operator, right),
                    result_type,
                )
            }
            Self::Comparison(left, operator, right) => {
                let operator = *operator;

                let left = Self::perform_semantic_analysis(*left, parsed_arena, typed_arena, ctx);
                let right = Self::perform_semantic_analysis(*right, parsed_arena, typed_arena, ctx);

                let left = left?;
                let right = right?;

                let left_type = typed_arena.get_expression_type(left);
                let right_type = typed_arena.get_expression_type(right);

                if !left_type.can_perform_comparison(operator, right_type) {
                    let span = parsed_arena.get_expression_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::CannotPerformComparisonOperation {
                            left: left_type.clone(),
                            operator,
                            right: right_type.clone(),
                        },
                    );
                }

                typed_arena.allocate_expression(
                    TypedExpression::Comparison(left, operator, right),
                    SemanticDataType::Boolean,
                )
            }
            Self::Logical(left, operator, right) => {
                let operator = *operator;

                let left_span = parsed_arena.get_expression_span(*left);
                let right_span = parsed_arena.get_expression_span(*right);

                let left = Self::perform_semantic_analysis(*left, parsed_arena, typed_arena, ctx);
                let right = Self::perform_semantic_analysis(*right, parsed_arena, typed_arena, ctx);

                let left = left?;
                let right = right?;

                let left_type = typed_arena.get_expression_type(left);
                let right_type = typed_arena.get_expression_type(right);

                let mut failed = false;

                if left_type
                    .assert_equals(ctx, left_span, &SemanticDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if right_type
                    .assert_equals(ctx, right_span, &SemanticDataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if failed {
                    return None;
                }

                typed_arena.allocate_expression(
                    TypedExpression::Logical(left, operator, right),
                    SemanticDataType::Boolean,
                )
            }
            Self::AugmentedAssignment(target, operator_span, operator, value) => {
                let operator_span = *operator_span;
                let operator = *operator;

                let target =
                    Self::as_place_semantic_analysis(*target, parsed_arena, typed_arena, ctx);
                let value = Self::perform_semantic_analysis(*value, parsed_arena, typed_arena, ctx);

                let target = target?;
                let value = value?;

                let target_type = typed_arena.get_place_expression_type(target);
                let value_type = typed_arena.get_expression_type(value);

                if target_type.get_arithmetic_result(value_type).is_none() {
                    return ctx.add_error(
                        operator_span,
                        SemanticAnalysisError::CannotPerformArithmeticOperation {
                            left: target_type.clone(),
                            operator,
                            right: value_type.clone(),
                        },
                    );
                }

                typed_arena.allocate_expression(
                    TypedExpression::AugmentedAssignment(target, operator, value),
                    SemanticDataType::Unit,
                )
            }
            Self::Assignment(target, value) => {
                let target = Self::as_assignee_perform_semantic_analysis(
                    *target,
                    parsed_arena,
                    typed_arena,
                    ctx,
                );

                let value_span = parsed_arena.get_expression_span(*value);

                let value = Self::perform_semantic_analysis(*value, parsed_arena, typed_arena, ctx);

                let target = target?;
                let value = value?;

                let value_type = typed_arena.get_expression_type(value);

                ParsedAssigneeExpression::perform_assignment_semantic_analysis(
                    target,
                    typed_arena,
                    ctx,
                    value_span,
                    value_type,
                )?;

                typed_arena.allocate_expression(
                    TypedExpression::Assignment(target, value),
                    SemanticDataType::Unit,
                )
            }
            Self::List(expressions) => {
                let expressions = expressions
                    .iter()
                    .copied()
                    .map(|expression| {
                        let expression_span = parsed_arena.get_expression_span(expression);

                        let expression = Self::perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some((expression_span, expression))
                    })
                    .collect_option_all::<Vec<_>>()?;

                let element_type =
                    if let Some((_, element_expression)) = expressions.first().copied() {
                        let element_type = typed_arena.get_expression_type(element_expression);

                        let mut element_type = element_type.clone();

                        let mut has_error = false;

                        for (span, expression) in expressions.iter().copied() {
                            let expression_type = typed_arena.get_expression_type(expression);

                            let Some(reduced_element_type) =
                                element_type.clone().reduce(expression_type)
                            else {
                                has_error = true;

                                ctx.add_error_unit(
                                    span,
                                    SemanticAnalysisError::MismatchedTypes {
                                        expected: element_type.clone(),
                                        actual: expression_type.clone(),
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
                        SemanticDataType::Inferred
                    };

                let expressions = expressions
                    .into_iter()
                    .map(|(_, expression)| expression)
                    .collect();

                typed_arena.allocate_expression(
                    TypedExpression::List(expressions),
                    SemanticDataType::List(Box::new(element_type)),
                )
            }
            Self::Compound(compound_values) => {
                let compound_values = compound_values
                    .iter()
                    .map(|(key, value)| {
                        let value = Self::perform_semantic_analysis(
                            *value,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some((key.clone(), value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                let compound_data_types = compound_values
                    .iter()
                    .map(|(key, value)| {
                        let value_type = typed_arena.get_expression_type(*value);

                        (key.clone(), value_type.clone())
                    })
                    .collect();

                typed_arena.allocate_expression(
                    TypedExpression::Compound(compound_values),
                    SemanticDataType::TypedCompound(compound_data_types),
                )
            }
            Self::PlayerScore(score) => {
                let score =
                    score
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_expression(
                    TypedExpression::Score(score),
                    SemanticDataType::Score(Box::new(SemanticDataType::Integer)),
                )
            }
            Self::Data(target_path) => {
                let (target, path) = &**target_path;

                let target =
                    target
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let path = path
                    .clone()
                    .perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let target = target?;
                let path = path?;

                typed_arena.allocate_expression(
                    TypedExpression::Data(Box::new(TypedData { target, path })),
                    SemanticDataType::Data(Box::new(SemanticDataType::Inferred)),
                )
            }
            Self::Condition(inverted, condition) => {
                let inverted = *inverted;

                let condition =
                    condition
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_expression(
                    TypedExpression::Condition(inverted, Box::new(condition)),
                    SemanticDataType::Boolean,
                )
            }
            Self::Command(command) => {
                let command =
                    command
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_expression(
                    TypedExpression::Command(Box::new(command)),
                    SemanticDataType::Integer,
                )
            }
            Self::Index(target, index) => {
                let target_span = parsed_arena.get_expression_span(*target);

                let target =
                    Self::perform_semantic_analysis(*target, parsed_arena, typed_arena, ctx);
                let index = Self::perform_semantic_analysis(*index, parsed_arena, typed_arena, ctx);

                let target = target?;
                let index = index?;

                let target_type = typed_arena.get_expression_type(target);
                let index_type = typed_arena.get_expression_type(index);

                let index_result =
                    target_type.get_index_result_semantic_analysis(ctx, target_span, index_type)?;

                typed_arena.allocate_expression(TypedExpression::Index(target, index), index_result)
            }
            Self::MethodCall {
                receiver,
                callee,
                arguments,
            } => {
                let receiver =
                    Self::perform_semantic_analysis(*receiver, parsed_arena, typed_arena, ctx);
                let _callee = callee.clone().perform_semantic_analysis(ctx);
                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|expression| {
                        Self::perform_semantic_analysis(expression, parsed_arena, typed_arena, ctx)
                    })
                    .collect_option_all::<Vec<_>>(); // TODO: Remove turbofish

                let _receiver = receiver?;
                let _arguments = arguments?;

                todo!()
            }
            Self::FieldAccess(expression, field_span, field) => {
                let place =
                    Self::perform_semantic_analysis(*expression, parsed_arena, typed_arena, ctx)?;

                let place_type = typed_arena.get_expression_type(place);

                let field_result =
                    place_type.get_field_result_semantic_analysis(ctx, *field_span, field)?;

                typed_arena.allocate_expression(
                    TypedExpression::FieldAccess(place, field.clone()),
                    field_result,
                )
            }
            Self::AsCast(expression, data_type) => {
                let expression =
                    Self::perform_semantic_analysis(*expression, parsed_arena, typed_arena, ctx);
                let data_type = data_type.clone().perform_semantic_analysis(ctx);

                let expression = expression?;

                let expression_type = typed_arena.get_expression_type(expression);

                if !expression_type.can_cast_to(&data_type) {
                    let span = parsed_arena.get_expression_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::CannotCastType {
                            from: expression_type.clone(),
                            to: data_type,
                        },
                    );
                }

                typed_arena.allocate_expression(
                    TypedExpression::AsCast(expression, data_type.clone()),
                    data_type,
                )
            }
            Self::ToCast(expression, runtime_storage_type) => {
                let (scale, expression) = Self::extract_scale(*expression, parsed_arena);

                let expression_span = parsed_arena.get_expression_span(expression);

                let expression =
                    Self::perform_semantic_analysis(expression, parsed_arena, typed_arena, ctx)?;

                let expression_type = typed_arena.get_expression_type(expression);

                let runtime_storage_type = *runtime_storage_type;

                let data_type = match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if !expression_type.can_be_assigned_to_score() {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotScoreCompatible(
                                    expression_type.clone(),
                                ),
                            );
                        }

                        SemanticDataType::Score(Box::new(expression_type.clone()))
                    }
                    RuntimeStorageType::Data => {
                        let Some(data_type) =
                            expression_type.get_data_type(&ctx.semantic_environment)
                        else {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotDataCompatible(
                                    expression_type.clone(),
                                ),
                            );
                        };

                        SemanticDataType::Data(Box::new(data_type))
                    }
                };

                typed_arena.allocate_expression(
                    TypedExpression::ToCast(scale, expression, runtime_storage_type),
                    data_type,
                )
            }
            Self::Tuple(expressions) => {
                let expressions = expressions
                    .iter()
                    .copied()
                    .map(|expression| {
                        Self::perform_semantic_analysis(expression, parsed_arena, typed_arena, ctx)
                    })
                    .collect_option_all::<Vec<_>>()?;

                let expression_types = expressions
                    .iter()
                    .copied()
                    .map(|expression| typed_arena.get_expression_type(expression).clone())
                    .collect();

                typed_arena.allocate_expression(
                    TypedExpression::Tuple(expressions),
                    SemanticDataType::Tuple(expression_types),
                )
            }
            Self::RegularStruct(path, field_values) => {
                let mut path = path.clone().perform_semantic_analysis(ctx);

                let id = ctx.get_visible_type_id(&path)?;

                let last_segment = path.segments.pop().unwrap();

                let (_, _, declaration) =
                    ctx.get_visible_regular_struct(last_segment.name_span, &last_segment.name, id)?;

                let id = HighRegularStructId(id.0);

                let data_type =
                    SemanticDataType::Struct(id.into(), last_segment.generic_types.clone());

                let generic_ids = declaration.generic_ids.clone();
                let field_types = declaration.field_types.clone();

                let field_values = field_values
                    .iter()
                    .map(|((key_span, field), value)| {
                        let Some(field_type) = field_types.get(field) else {
                            return ctx.add_error(
                                *key_span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: data_type.clone(),
                                    field: field.clone(),
                                },
                            );
                        };

                        let field_type = field_type
                            .clone()
                            .substitute_generics(&generic_ids, &last_segment.generic_types);

                        let value_span = parsed_arena.get_expression_span(*value);

                        let value = Self::perform_semantic_analysis(
                            *value,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        let value_type = typed_arena.get_expression_type(value);

                        if !value_type.equals(&field_type) {
                            return ctx.add_error(
                                value_span,
                                SemanticAnalysisError::MismatchedTypes {
                                    expected: field_type,
                                    actual: value_type.clone(),
                                },
                            );
                        }

                        Some((field.clone(), value))
                    })
                    .collect_option_all::<HashMap<_, _>>()?;

                let mut has_error = false;

                for declared_field_name in field_types.keys() {
                    if !field_values
                        .keys()
                        .any(|given_field_name| given_field_name == declared_field_name)
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

                typed_arena.allocate_expression(
                    TypedExpression::RegularStruct(id, last_segment.generic_types, field_values),
                    data_type,
                )
            }
            Self::Call { callee, arguments } => {
                let callee_span = parsed_arena.get_expression_span(*callee);

                let callee =
                    Self::perform_semantic_analysis(*callee, parsed_arena, typed_arena, ctx);

                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|argument| {
                        let argument_span = parsed_arena.get_expression_span(argument);

                        let argument = Self::perform_semantic_analysis(
                            argument,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some((argument_span, argument))
                    })
                    .collect_option_all::<Vec<_>>();

                let callee = callee?;
                let arguments = arguments?;

                let callee_type = typed_arena.get_expression_type(callee);

                let Some(call_info) = callee_type.get_call_info(&ctx.semantic_environment)? else {
                    return ctx
                        .add_error(callee_span, SemanticAnalysisError::ExpressionIsNotCallable);
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

                for (data_type, (argument_span, argument)) in
                    call_info.parameter_types.into_iter().zip(arguments)
                {
                    let argument_type = typed_arena.get_expression_type(argument);

                    if argument_type.equals(&data_type) {
                        if !failed {
                            new_arguments.push(argument);
                        }
                    } else {
                        ctx.add_error_unit(
                            argument_span,
                            SemanticAnalysisError::MismatchedTypes {
                                expected: data_type,
                                actual: argument_type.clone(),
                            },
                        );

                        failed = true;
                    }
                }

                if failed {
                    return None;
                }

                typed_arena.allocate_expression(
                    TypedExpression::Call(callee, new_arguments),
                    call_info.return_type,
                )
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                let condition_span = parsed_arena.get_expression_span(*condition);

                let condition =
                    Self::perform_semantic_analysis(*condition, parsed_arena, typed_arena, ctx);

                let body = body
                    .clone()
                    .perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let else_body = else_body.map(|else_body| {
                    let span = parsed_arena.get_expression_span(else_body);

                    let expression =
                        Self::perform_semantic_analysis(else_body, parsed_arena, typed_arena, ctx)?;

                    Some((span, expression))
                });

                let condition = condition?;

                let condition_type = typed_arena.get_expression_type(condition);

                if !condition_type.is_condition() {
                    return ctx.add_error(
                        condition_span,
                        SemanticAnalysisError::TypeIsNotCondition(condition_type.clone()),
                    );
                }

                let (body_span, tail_expression_span, body) = body?;

                let body_type = typed_arena.get_expression_type(body);

                let else_body = match else_body {
                    Some(else_body) => Some(else_body?),
                    None => None,
                };

                let else_type = if let Some((else_body_span, else_body)) = else_body {
                    let else_body_type = typed_arena.get_expression_type(else_body);

                    else_body_type.assert_equals(ctx, else_body_span, body_type)?;

                    else_body_type
                } else {
                    body_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(body_span),
                        &SemanticDataType::Unit,
                    )?;

                    &SemanticDataType::Unit
                };

                let data_type = body_type.clone().reduce(else_type).unwrap();

                typed_arena.allocate_expression(
                    TypedExpression::If {
                        condition,
                        body,
                        else_body: else_body.map(|(_, else_body)| else_body),
                    },
                    data_type,
                )
            }
            Self::Block(expression) => {
                let (_, _, expression) =
                    expression
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                expression
            }
            Self::WhileLoop(condition, body) => {
                let condition_span = parsed_arena.get_expression_span(*condition);

                let condition =
                    Self::perform_semantic_analysis(*condition, parsed_arena, typed_arena, ctx);

                ctx.loop_depth += 1;
                let body = body
                    .clone()
                    .perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                ctx.loop_depth -= 1;

                let condition = condition?;

                let condition_type = typed_arena.get_expression_type(condition);

                condition_type.assert_condition(ctx, condition_span)?;

                let (_, _, body) = body?;

                typed_arena.allocate_expression(
                    TypedExpression::WhileLoop(condition, body),
                    SemanticDataType::Unit,
                )
            }
            Self::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body
                    .clone()
                    .perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                ctx.loop_depth -= 1;

                let (_, _, body) = body?;

                typed_arena.allocate_expression(TypedExpression::Loop(body), SemanticDataType::Unit)
            }
            Self::ForLoop(reversed, pattern, iterable, body) => {
                let reversed = *reversed;

                let iterable_span = parsed_arena.get_expression_span(*iterable);

                let iterable =
                    Self::perform_semantic_analysis(*iterable, parsed_arena, typed_arena, ctx)?;

                let iterable_type = typed_arena.get_expression_type(iterable);

                let Some(iterable_type) = iterable_type
                    .clone()
                    .get_iterable_type_semantic_analysis(ctx, iterable_span)
                else {
                    Pattern::destructure_unknown(*pattern, parsed_arena, ctx);

                    return None;
                };

                ctx.enter_scope();

                let Some(pattern) = Pattern::perform_semantic_analysis(
                    *pattern,
                    parsed_arena,
                    typed_arena,
                    ctx,
                    &iterable_type,
                ) else {
                    ctx.exit_scope();

                    return None;
                };

                ctx.loop_depth += 1;
                let Some((_, _, body)) =
                    body.clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)
                else {
                    ctx.loop_depth -= 1;

                    ctx.exit_scope();

                    return None;
                };
                ctx.loop_depth -= 1;
                ctx.exit_scope();

                // TODO: Reorder semantic analysis

                typed_arena.allocate_expression(
                    TypedExpression::ForLoop(reversed, pattern, iterable, body),
                    SemanticDataType::Unit,
                )
            }
            Self::Path(path) => {
                let mut path = path.clone().perform_semantic_analysis(ctx);

                let id = ctx.get_visible_value_id(&path)?;

                let value_declaration = ctx.semantic_environment.get_value(id).clone();

                let last_segment = path.segments.pop().unwrap();

                let (id, data_type) = value_declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_types.clone(),
                    last_segment.name_span,
                )?;

                typed_arena.allocate_expression(
                    TypedExpression::Value(id, last_segment.generic_types),
                    data_type,
                )
            }
            Self::Boolean(value) => typed_arena
                .allocate_expression(TypedExpression::Boolean(*value), SemanticDataType::Boolean),
            Self::Byte(value) => typed_arena
                .allocate_expression(TypedExpression::Byte(*value), SemanticDataType::Byte),
            Self::Short(value) => typed_arena
                .allocate_expression(TypedExpression::Short(*value), SemanticDataType::Short),
            Self::Integer(value) => typed_arena
                .allocate_expression(TypedExpression::Integer(*value), SemanticDataType::Integer),
            Self::InferredInteger(value) => typed_arena.allocate_expression(
                TypedExpression::InferredInteger(*value),
                SemanticDataType::InferredInteger,
            ),
            Self::Long(value) => typed_arena
                .allocate_expression(TypedExpression::Long(*value), SemanticDataType::Long),
            Self::Float(value) => typed_arena
                .allocate_expression(TypedExpression::Float(*value), SemanticDataType::Float),
            Self::InferredFloat(value) => typed_arena.allocate_expression(
                TypedExpression::InferredFloat(*value),
                SemanticDataType::InferredFloat,
            ),
            Self::Double(value) => typed_arena
                .allocate_expression(TypedExpression::Double(*value), SemanticDataType::Double),
            Self::String(value) => typed_arena.allocate_expression(
                TypedExpression::String(value.clone()),
                SemanticDataType::String,
            ),
            Self::Underscore => {
                let span = parsed_arena.get_expression_span(id);

                return ctx.add_error(span, SemanticAnalysisError::UnderscoreExpression);
            }
            Self::Unit => {
                typed_arena.allocate_expression(TypedExpression::Unit, SemanticDataType::Unit)
            }
            Self::ResourceLocation(resource_location) => {
                let resource_location = resource_location.clone().perform_semantic_analysis(
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                typed_arena.allocate_expression(
                    TypedExpression::ResourceLocation(Box::new(resource_location)),
                    SemanticDataType::ResourceLocation,
                )
            }
            Self::EntitySelector(selector) => {
                let selector =
                    selector
                        .clone()
                        .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_expression(
                    TypedExpression::EntitySelector(Box::new(selector)),
                    SemanticDataType::EntitySelector,
                )
            }
            Self::Coordinates(coordinates) => {
                let coordinates = coordinates.clone().perform_semantic_analysis(
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                typed_arena.allocate_expression(
                    TypedExpression::Coordinates(Box::new(coordinates)),
                    SemanticDataType::Coordinates,
                )
            }
            Self::Return(keyword_span, expression_span, expression) => {
                let expression = match expression {
                    Some(expression) => Self::perform_semantic_analysis(
                        *expression,
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?,
                    None => typed_arena
                        .allocate_expression(TypedExpression::Unit, SemanticDataType::Unit),
                };

                let expression_type = typed_arena.get_expression_type(expression);

                let context = ctx.function_contexts.last().unwrap();

                let return_type = context.return_type();

                if !expression_type.equals(return_type) {
                    return ctx.add_error(
                        *expression_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: return_type.clone(),
                            actual: expression_type.clone(),
                        },
                    );
                }

                let context = ctx.function_contexts.last().unwrap();

                if let Some(is_runtime) = context.is_runtime()
                    && !is_runtime
                {
                    return ctx.add_error(
                        *keyword_span,
                        SemanticAnalysisError::CannotUseReturnInCompiletimeFunction,
                    );
                }

                typed_arena.allocate_expression(
                    TypedExpression::Return(expression),
                    SemanticDataType::Never,
                )
            }
        })
    }
}
