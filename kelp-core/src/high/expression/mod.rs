use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;
use ordered_float::NotNan;

use crate::{
    high::{
        command::{Command, execute::subcommand::r#if::ExecuteIfSubcommand},
        coordinate::Coordinates,
        data::DataTarget,
        data_type::unresolved::UnresolvedDataType,
        entity_selector::EntitySelector,
        expression::block::BlockExpression,
        nbt_path::NbtPath,
        pattern::Pattern,
        player_score::PlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    low::{
        data_type::DataType,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionKind},
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    path::generic::GenericPath,
    place::{PlaceType, PlaceTypeKind},
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
    AsCast(Box<Expression>, UnresolvedDataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Path(GenericPath<UnresolvedDataType>),
    StructStruct(
        GenericPath<UnresolvedDataType>,
        HashMap<SNBTString, Expression>,
    ),
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
    pub fn get_place_type(&self, ctx: &mut SemanticAnalysisContext) -> Option<Option<PlaceType>> {
        let (data_type, place_type_kind) = match &self.kind {
            ExpressionKind::Tuple(expressions) => {
                let Some(place_types) = expressions
                    .iter()
                    .map(|expression| expression.get_place_type(ctx))
                    .collect_option_all::<Option<Vec<_>>>()?
                else {
                    return Some(None);
                };

                let data_types = place_types
                    .iter()
                    .map(|place_type| place_type.data_type.clone())
                    .collect();

                (
                    DataType::Tuple(data_types),
                    PlaceTypeKind::Tuple(place_types),
                )
            }
            ExpressionKind::PlayerScore(_) => (
                DataType::Score(Box::new(DataType::Integer)),
                PlaceTypeKind::Score(DataType::Integer),
            ),
            ExpressionKind::Data(_) => (
                DataType::Data(Box::new(DataType::Inferred)),
                PlaceTypeKind::Data(DataType::Inferred),
            ),
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                let Some(place_type) = expression.get_place_type(ctx)? else {
                    return Some(None);
                };

                let data_type = place_type
                    .data_type
                    .clone()
                    .get_dereferenced_result_semantic_analysis(ctx, expression.span)?;

                (data_type, PlaceTypeKind::Dereference(Box::new(place_type)))
            }
            ExpressionKind::Index(target, _) => {
                let Some(target_place_type) = target.get_place_type(ctx)? else {
                    return Some(None);
                };

                let data_type = target_place_type
                    .data_type
                    .clone()
                    .get_index_result_semantic_analysis(ctx, target.span)?;

                (data_type, PlaceTypeKind::Index(Box::new(target_place_type)))
            }
            ExpressionKind::FieldAccess(target, field) => {
                let Some(target_place_type) = target.get_place_type(ctx)? else {
                    return Some(None);
                };

                let data_type = target_place_type
                    .data_type
                    .get_field_result_semantic_analysis(ctx, field.span, &field.snbt_string.1)?;

                (
                    data_type,
                    PlaceTypeKind::FieldAccess(
                        Box::new(target_place_type),
                        field.span,
                        field.snbt_string.1.clone(),
                    ),
                )
            }
            ExpressionKind::Underscore => (DataType::Inferred, PlaceTypeKind::Underscore),
            ExpressionKind::Path(path) => {
                let Some(id) = ctx.get_visible_value_id(path) else {
                    return Some(None);
                };

                let declaration = ctx.get_value(id);

                let Some(data_type) = declaration.data_type()? else {
                    return Some(None);
                };

                (data_type.clone(), PlaceTypeKind::Value)
            }
            _ => {
                return ctx.add_error(self.span, SemanticAnalysisError::CannotBeAssignedTo);
            }
        };

        Some(Some(PlaceType {
            span: self.span,
            data_type,
            kind: place_type_kind,
        }))
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
                        .with(DataType::Reference(Box::new(expression_data_type)))
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
                    .with(DataType::Boolean)
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let (left_span, left) = left?;
                let (right_span, right) = right?;

                let mut failed = false;

                if left
                    .data_type
                    .assert_equals(ctx, left_span, &DataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if right
                    .data_type
                    .assert_equals(ctx, right_span, &DataType::Boolean)
                    .is_none()
                {
                    failed = true;
                }

                if failed {
                    return None;
                }

                UnresolvedExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                    .with(DataType::Boolean)
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let place = target.get_place_type(ctx)?;

                let place = place?;

                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                place.perform_augmented_assignment_semantic_analysis(
                    ctx,
                    operator,
                    value_span,
                    &value.data_type,
                )?;

                let (_, target) = target.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::AugmentedAssignment(
                    Box::new(target),
                    operator,
                    Box::new(value),
                )
                .with(DataType::Unit)
            }
            ExpressionKind::Assignment(target, value) => {
                let place = target.get_place_type(ctx)?;

                let place = place?;

                let (value_span, value) = value.perform_semantic_analysis(ctx)?;

                place.perform_assignment_semantic_analysis(ctx, value_span, &value.data_type)?;

                ctx.is_lhs = true;
                let (_, target) = target.perform_semantic_analysis(ctx)?;
                ctx.is_lhs = false;

                UnresolvedExpressionKind::Assignment(Box::new(target), Box::new(value))
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

                UnresolvedExpressionKind::List(expressions)
                    .with(DataType::List(Box::new(element_type)))
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
                    .with(DataType::TypedCompound(compound_data_types))
            }
            ExpressionKind::PlayerScore(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::PlayerScore(score)
                    .with(DataType::Score(Box::new(DataType::Integer)))
            }
            ExpressionKind::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                UnresolvedExpressionKind::Data(Box::new((target, path)))
                    .with(DataType::Data(Box::new(DataType::Inferred)))
            }
            ExpressionKind::Condition(inverted, condition) => {
                let condition = condition.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Condition(inverted, Box::new(condition))
                    .with(DataType::Boolean)
            }
            ExpressionKind::Command(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Command(Box::new(command)).with(DataType::Integer)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.perform_semantic_analysis(ctx);
                let index = index.perform_semantic_analysis(ctx);

                let (target_span, target) = target?;
                let (index_span, index) = index?;

                let index_result = target
                    .data_type
                    .clone()
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

                        DataType::Score(Box::new(expression.data_type.clone()))
                    }
                    RuntimeStorageType::Data => {
                        let Some(data_type) = expression.data_type.get_data_type(&ctx.environment)
                        else {
                            return ctx.add_error(
                                expression_span,
                                SemanticAnalysisError::TypeIsNotDataCompatible(
                                    expression.data_type,
                                ),
                            );
                        };

                        DataType::Data(Box::new(data_type))
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
                    .with(DataType::Tuple(expression_data_types))
            }
            ExpressionKind::StructStruct(path, field_values) => {
                let mut path = path.resolve_fully(ctx)?;

                let id = ctx.get_visible_type_id(&path)?;

                let type_declaration = ctx.get_type(id).clone();

                let last_segment = path.segments.pop().unwrap();

                let data_type = type_declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_spans,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?;

                let id = data_type.as_struct_id_semantic_analysis(
                    ctx,
                    last_segment.name_span,
                    &last_segment.name,
                )?;

                let (specific_id, _, declaration) =
                    ctx.get_visible_struct_struct(last_segment.name_span, &last_segment.name, id)?;

                let field_types = declaration.field_types.clone();

                let field_values = field_values
                    .into_iter()
                    .map(|(key, value)| {
                        let Some(field_type) = field_types.get(&key.snbt_string.1) else {
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

                let mut has_error = false;

                for declared_field_name in field_types.into_keys() {
                    if !field_values
                        .keys()
                        .any(|given_field_name| given_field_name.1 == declared_field_name)
                    {
                        has_error = true;

                        ctx.add_error::<()>(
                            path.span,
                            SemanticAnalysisError::MissingField(declared_field_name.clone()),
                        );
                    }
                }

                if has_error {
                    return None;
                }

                UnresolvedExpressionKind::StructStruct(specific_id, field_values)
                    .with(DataType::Struct(id))
            }
            ExpressionKind::Call { callee, arguments } => {
                let (callee_span, callee) = callee.perform_semantic_analysis(ctx)?;

                let arguments = arguments
                    .into_iter()
                    .map(|argument| argument.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>()?;

                let Some(call_info) = callee.data_type.get_call_info(&ctx.environment) else {
                    return ctx
                        .add_error(callee_span, SemanticAnalysisError::ExpressionIsNotCallable);
                };

                let parameter_count = call_info.parameters.len();
                let argument_count = arguments.len();

                if argument_count != parameter_count {
                    return ctx.add_error(
                        callee_span,
                        SemanticAnalysisError::InvalidParameterCount {
                            expected: parameter_count,
                            actual: argument_count,
                        },
                    );
                }

                let mut failed = false;

                let mut new_arguments = Vec::with_capacity(arguments.len());

                for ((_, data_type), (argument_span, argument)) in
                    call_info.parameters.into_iter().zip(arguments.into_iter())
                {
                    let Some(data_type) = data_type else {
                        continue;
                    };

                    if argument.data_type.equals(&data_type) {
                        if !failed {
                            new_arguments.push(argument);
                        }
                    } else {
                        ctx.add_error::<()>(
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
                        &DataType::Unit,
                    )?;

                    &DataType::Unit
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
                    .with(DataType::Unit)
            }
            ExpressionKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (_, _, body) = body?;

                UnresolvedExpressionKind::Loop(Box::new(body)).with(DataType::Unit)
            }
            ExpressionKind::ForLoop(reversed, pattern, iterable, body) => {
                let (expression_span, iterable) = iterable.perform_semantic_analysis(ctx)?;

                let Some(iterable_type) = iterable
                    .data_type
                    .get_iterable_type_semantic_analysis(ctx, expression_span)
                else {
                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                ctx.enter_scope();

                let Some(pattern) = pattern.perform_semantic_analysis(ctx, iterable_type) else {
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
                .with(DataType::Unit)
            }
            ExpressionKind::Path(path) => {
                let mut path = path.resolve_fully(ctx)?;

                let id = ctx.get_visible_value_id(&path)?;

                let value_declaration = ctx.get_value(id).clone();

                let last_segment = path.segments.pop().unwrap();

                let (id, data_type) = value_declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?;

                UnresolvedExpressionKind::Value(id).with(data_type)
            }
            ExpressionKind::Boolean(value) => {
                UnresolvedExpressionKind::Boolean(value).with(DataType::Boolean)
            }
            ExpressionKind::Byte(value) => {
                UnresolvedExpressionKind::Byte(value).with(DataType::Byte)
            }
            ExpressionKind::Short(value) => {
                UnresolvedExpressionKind::Short(value).with(DataType::Short)
            }
            ExpressionKind::Integer(value) => {
                UnresolvedExpressionKind::Integer(value).with(DataType::Integer)
            }
            ExpressionKind::InferredInteger(value) => {
                UnresolvedExpressionKind::InferredInteger(value).with(DataType::InferredInteger)
            }
            ExpressionKind::Long(value) => {
                UnresolvedExpressionKind::Long(value).with(DataType::Long)
            }
            ExpressionKind::Float(value) => {
                UnresolvedExpressionKind::Float(value).with(DataType::Float)
            }
            ExpressionKind::InferredFloat(value) => {
                UnresolvedExpressionKind::InferredFloat(value).with(DataType::InferredFloat)
            }
            ExpressionKind::Double(value) => {
                UnresolvedExpressionKind::Double(value).with(DataType::Double)
            }
            ExpressionKind::String(value) => {
                UnresolvedExpressionKind::String(value.snbt_string).with(DataType::String)
            }
            ExpressionKind::Underscore => {
                if !ctx.is_lhs {
                    return ctx.add_error(self.span, SemanticAnalysisError::UnderscoreExpression);
                }

                UnresolvedExpressionKind::Underscore.with(DataType::Inferred)
            }
            ExpressionKind::Unit => UnresolvedExpressionKind::Unit.with(DataType::Unit),
            ExpressionKind::ResourceLocation(resource_location) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::ResourceLocation(Box::new(resource_location))
                    .with(DataType::ResourceLocation)
            }
            ExpressionKind::EntitySelector(selector) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::EntitySelector(Box::new(selector))
                    .with(DataType::EntitySelector)
            }
            ExpressionKind::Coordinates(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                UnresolvedExpressionKind::Coordinates(Box::new(coordinates))
                    .with(DataType::Coordinates)
            }
            ExpressionKind::Return(keyword_span, expression_span, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        expression
                    }
                    None => UnresolvedExpressionKind::Unit.with(DataType::Unit),
                };

                if let Some(Some(return_type)) = ctx.function_return_types.last()
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

                if let Some(return_type) = return_type
                    && return_type.is_compiletime(&ctx.environment)
                {
                    return ctx.add_error(
                        keyword_span,
                        SemanticAnalysisError::CannotUseReturnWithCompiletimeResult,
                    );
                }

                UnresolvedExpressionKind::Return(Box::new(expression)).with(DataType::Never)
            }
        };

        Some((self.span, expression))
    }
}
