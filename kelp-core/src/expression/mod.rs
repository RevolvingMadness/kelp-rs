use std::collections::BTreeMap;

use minecraft_command_types::command::{
    Command,
    enums::store_type::StoreType,
    execute::{ExecuteStoreSubcommand, ExecuteSubcommand},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data_type::{DataTypeKind, high::HighDataType},
    datapack::HighDatapack,
    expression::{
        constant::ResolvedExpression, supports_variable_type_scope::SupportsVariableTypeScope,
    },
    high::{
        command::{HighCommand, execute::subcommand::r#if::HighExecuteIfSubcommand},
        data::HighDataTarget,
        nbt_path::HighNbtPath,
        player_score::HighPlayerScore,
        snbt_string::HighSNBTString,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::{Place, PlaceType, PlaceTypeKind},
    runtime_storage_type::RuntimeStorageType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::OptionUnitIterExt,
};

pub mod constant;
pub mod literal;
pub mod supports_variable_type_scope;
pub mod utils;

pub type ExpressionCompoundKind = BTreeMap<HighSNBTString, Expression>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(NotNan<f32>),
    Double(NotNan<f64>),
    String(HighSNBTString),
    Underscore,
    Unit,
    Unary(UnaryOperator, Box<Expression>),
    Arithmetic(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    AugmentedAssignment(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
    Compound(ExpressionCompoundKind),
    PlayerScore(HighPlayerScore),
    Data(HighDataTarget, HighNbtPath),
    Condition(bool, HighExecuteIfSubcommand),
    Command(Box<HighCommand>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, HighSNBTString),
    AsCast(Box<Expression>, HighDataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    Variable(String),
    Struct(
        Span,
        String,
        Vec<HighDataType>,
        BTreeMap<HighSNBTString, Expression>,
    ),
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
    pub const fn is_index_out_of_bounds(&self, index: &Expression) -> Option<bool> {
        let Self::List(expressions) = self else {
            return None;
        };

        let Expression {
            kind: Self::Integer(index),
            ..
        } = index
        else {
            return None;
        };

        Some((*index as usize) >= expressions.len())
    }

    pub fn as_place(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Place> {
        match self {
            Self::Underscore => Some(Place::Underscore),
            Self::Unary(operator, expression) => match operator {
                UnaryOperator::Negate | UnaryOperator::Reference | UnaryOperator::Invert => None,
                UnaryOperator::Dereference => {
                    let expression = expression.resolve(datapack, ctx);

                    Some(Place::Dereference(Box::new(expression)))
                }
            },
            Self::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                Some(Place::Score(score))
            }
            Self::Data(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Some(Place::Data(target, path))
            }
            Self::Index(target, index) => {
                let target = target.resolve(datapack, ctx);
                let index = index.resolve(datapack, ctx);

                Some(Place::Index(Box::new(target), Box::new(index)))
            }
            Self::FieldAccess(target, field) => {
                let target = target.resolve(datapack, ctx);

                Some(Place::Field(Box::new(target), field.snbt_string.1))
            }
            Self::Tuple(tuple) => Some(Place::Tuple(
                tuple
                    .into_iter()
                    .map(|expression| expression.kind.as_place(datapack, ctx))
                    .collect::<Option<_>>()?,
            )),
            Self::Variable(name) => Some(Place::Variable(name)),
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Unit
            | Self::Arithmetic(_, _, _)
            | Self::Comparison(_, _, _)
            | Self::Logical(_, _, _)
            | Self::AugmentedAssignment(_, _, _)
            | Self::Assignment(_, _)
            | Self::List(_)
            | Self::Compound(_)
            | Self::Condition(_, _)
            | Self::Command(_)
            | Self::AsCast(_, _)
            | Self::ToCast(_, _)
            | Self::Struct(_, _, _, _) => None,
        }
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            Self::Underscore => DataTypeKind::Inferred,
            Self::Boolean(_)
            | Self::Comparison(_, _, _)
            | Self::Logical(_, _, _)
            | Self::Condition(_, _) => DataTypeKind::Boolean,
            Self::Byte(_) => DataTypeKind::Byte,
            Self::Short(_) => DataTypeKind::Short,
            Self::Integer(_) | Self::Command(_) => DataTypeKind::Integer,
            Self::Long(_) => DataTypeKind::Long,
            Self::Float(_) => DataTypeKind::Float,
            Self::Double(_) => DataTypeKind::Double,
            Self::String(_) => DataTypeKind::String,
            Self::Unit | Self::Assignment(_, _) | Self::AugmentedAssignment(_, _, _) => {
                DataTypeKind::Unit
            }
            Self::Unary(operator, expression) => {
                let expression_type = expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?;

                match operator {
                    UnaryOperator::Negate => expression_type.get_negated_result()?,
                    UnaryOperator::Reference => DataTypeKind::Reference(Box::new(expression_type)),
                    UnaryOperator::Dereference => expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?
                        .dereference()?,
                    UnaryOperator::Invert => expression_type.get_inverted_result()?,
                }
            }
            Self::Variable(name) => supports_variable_type_scope.get_variable(name)??,
            Self::Arithmetic(left, operator, right) => {
                let left_type = left.kind.infer_data_type(supports_variable_type_scope)?;
                let right_type = right.kind.infer_data_type(supports_variable_type_scope)?;

                left_type.get_arithmetic_result(
                    supports_variable_type_scope,
                    *operator,
                    &right_type,
                )?
            }
            Self::List(list) => {
                let element_type = list.first().map_or(DataTypeKind::Inferred, |first| {
                    first
                        .kind
                        .infer_data_type(supports_variable_type_scope)
                        .unwrap_or(DataTypeKind::Inferred)
                });

                DataTypeKind::List(Box::new(element_type))
            }
            Self::Compound(compound) => DataTypeKind::TypedCompound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| {
                        value
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                            .map(|data_type| (key.snbt_string, data_type))
                    })
                    .collect::<Option<_>>()?,
            ),
            Self::PlayerScore(_) => DataTypeKind::Score(Box::new(DataTypeKind::Integer)),
            Self::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::Inferred)),
            Self::Index(target, _) => target
                .kind
                .infer_data_type(supports_variable_type_scope)?
                .get_index_result()?,
            Self::FieldAccess(target, field) => target
                .kind
                .infer_data_type(supports_variable_type_scope)?
                .get_field_result(supports_variable_type_scope, &field.snbt_string.1)?,
            Self::AsCast(expression, data_type) => {
                let expression_type = expression
                    .kind
                    .infer_data_type(supports_variable_type_scope);

                data_type
                    .kind
                    .resolve(supports_variable_type_scope, None)?
                    .try_infer(expression_type)
            }
            Self::ToCast(expression, storage_type) => match storage_type {
                RuntimeStorageType::Score => {
                    let expression_type = expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?;

                    DataTypeKind::Score(Box::new(expression_type.to_score()?))
                }
                RuntimeStorageType::Data => {
                    let expression_type = expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?;

                    DataTypeKind::Data(Box::new(expression_type.to_data()))
                }
            },
            Self::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
            Self::Struct(_, name, generic_types, _) => {
                let declaration = supports_variable_type_scope.get_data_type(name)??;

                let resolved_generics = generic_types
                    .iter()
                    .map(|generic_type| {
                        generic_type
                            .kind
                            .resolve(supports_variable_type_scope, None)
                    })
                    .collect::<Option<Vec<_>>>()?;

                declaration.resolve(supports_variable_type_scope, resolved_generics)?
            }
        })
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
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
        expected_type: Option<&DataTypeKind>,
    ) -> Option<()> {
        match &self.kind {
            ExpressionKind::Unary(operator, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)?;

                let data_type = expression.kind.infer_data_type(ctx)?;

                match operator {
                    UnaryOperator::Negate => {
                        if data_type.get_negated_result().is_none() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: self.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotNegateType(data_type),
                                ),
                            });
                        }
                    }
                    UnaryOperator::Reference => {
                        let place_type = expression.get_place_type(ctx);

                        // TODO

                        if place_type.is_none() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeReferenced(data_type),
                                ),
                            });
                        }
                    }
                    UnaryOperator::Dereference => {
                        if !data_type.can_be_dereferenced() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeDereferenced(data_type),
                                ),
                            });
                        }
                    }
                    UnaryOperator::Invert => {
                        if data_type.get_inverted_result().is_none() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: self.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotInvertType(data_type),
                                ),
                            });
                        }
                    }
                }

                Some(())
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs, None);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs, None);

                let left_type = left.kind.infer_data_type(ctx)?;
                let right_type = right.kind.infer_data_type(ctx)?;

                let result_type = left_type.get_arithmetic_result(ctx, *operator, &right_type);

                if result_type.is_none() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformArithmeticOperation {
                                left: left_type,
                                operator: *operator,
                                right: right_type,
                            },
                        ),
                    });
                }

                if *operator == ArithmeticOperator::Swap
                    && (!left_type.is_lvalue() || !right_type.is_lvalue())
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformArithmeticOperation {
                                left: left_type,
                                operator: *operator,
                                right: right_type,
                            },
                        ),
                    });
                }

                left_result?;
                right_result?;

                Some(())
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs, None);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs, None);

                let left_type = left.kind.infer_data_type(ctx)?;
                let right_type = right.kind.infer_data_type(ctx)?;

                if let Some(value) = left_type.can_perform_comparison(ctx, *operator, &right_type)
                    && !value
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformComparisonOperation {
                                left: left_type,
                                operator: *operator,
                                right: right_type,
                            },
                        ),
                    });
                }

                left_result?;
                right_result?;

                Some(())
            }
            ExpressionKind::Logical(left, _, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs, None);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs, None);

                let left_type = left.kind.infer_data_type(ctx)?;
                let right_type = right.kind.infer_data_type(ctx)?;

                if left_type != DataTypeKind::Boolean {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: left.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: DataTypeKind::Boolean,
                                actual: left_type,
                            },
                        ),
                    });
                } else if right_type != DataTypeKind::Boolean {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: right.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: DataTypeKind::Boolean,
                                actual: right_type,
                            },
                        ),
                    });
                }

                left_result?;
                right_result?;

                Some(())
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                target.perform_semantic_analysis(ctx, is_lhs, None)?;

                let target_type = target.kind.infer_data_type(ctx)?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                value.perform_semantic_analysis(ctx, is_lhs, Some(&target_type))?;

                let value_data_type = value.kind.infer_data_type(ctx)?;

                place.perform_augmented_assignment_semantic_analysis(
                    ctx,
                    operator,
                    value,
                    &value_data_type,
                )?;

                Some(())
            }
            ExpressionKind::Assignment(target, value) => {
                target.perform_semantic_analysis(ctx, true, None)?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                value.perform_semantic_analysis(ctx, false, None)?;

                let value_data_type = value.kind.infer_data_type(ctx)?;

                place.perform_assignment_semantic_analysis(
                    ctx,
                    *value.clone(),
                    &value_data_type,
                )?;

                Some(())
            }
            ExpressionKind::List(expressions) => {
                let element_expected_type =
                    expected_type.and_then(|expected_type| match expected_type {
                        DataTypeKind::List(inner) => Some(inner.as_ref()),
                        _ => None,
                    });

                expressions
                    .iter()
                    .map(|expression| {
                        expression.perform_semantic_analysis(ctx, is_lhs, element_expected_type)
                    })
                    .all_some()?;

                if expected_type.is_none() {
                    if let Some(expression) = expressions.first() {
                        let element_type = expression.kind.infer_data_type(ctx)?;

                        expressions
                            .iter()
                            .map(|expression| {
                                let expression_type = expression.kind.infer_data_type(ctx)?;

                                element_type.perform_equality_semantic_analysis(
                                    ctx,
                                    &expression_type,
                                    expression,
                                )
                            })
                            .all_some()
                    } else {
                        Some(())
                    }
                } else {
                    Some(())
                }
            }
            ExpressionKind::Compound(compound) => {
                let expected_fields =
                    if let Some(DataTypeKind::TypedCompound(fields)) = expected_type {
                        Some(fields)
                    } else {
                        None
                    };

                compound
                    .iter()
                    .map(|(key, value)| {
                        let field_expected =
                            expected_fields.and_then(|fields| fields.get(&key.snbt_string));
                        value.perform_semantic_analysis(ctx, is_lhs, field_expected)
                    })
                    .all_some()
            }
            ExpressionKind::PlayerScore(score) => score.perform_semantic_analysis(ctx, is_lhs),
            ExpressionKind::Data(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                path_result?;

                Some(())
            }
            ExpressionKind::Condition(_, high_execute_if_subcommand) => {
                high_execute_if_subcommand.perform_semantic_analysis(ctx, is_lhs)
            }
            ExpressionKind::Command(command) => command.perform_semantic_analysis(ctx, is_lhs),
            ExpressionKind::Index(target, index) => {
                let target_result = target.perform_semantic_analysis(ctx, is_lhs, None);
                let index_result = index.perform_semantic_analysis(ctx, is_lhs, None);

                target_result?;

                let target_type = target.kind.infer_data_type(ctx)?;

                if target_type.get_index_result().is_none() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeIndexed(target_type),
                        ),
                    });
                }

                // TODO: Improve this.
                if target.kind.is_index_out_of_bounds(index) == Some(true) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: index.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::IndexOutOfBounds,
                        ),
                    });
                }

                index_result?;

                Some(())
            }
            ExpressionKind::FieldAccess(expression, field) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs, None);

                expression_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;

                if !expression_type.has_fields() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: field.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeDoesntHaveFields(expression_type),
                        ),
                    });
                }

                if expression_type
                    .get_field_result(ctx, &field.snbt_string.1)
                    .is_none()
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: field.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeDoesntHaveField {
                                data_type: expression_type,
                                field: field.snbt_string.1.clone(),
                            },
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs, None);
                let data_type_result = data_type.perform_semantic_analysis(None, ctx);

                expression_result?;
                data_type_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;
                let data_type = data_type.kind.resolve(ctx, None)?;

                if !expression_type.can_cast_to(&data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotCastType {
                                from: expression_type,
                                to: data_type,
                            },
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::ToCast(expression, runtime_storage) => {
                expression.perform_semantic_analysis(ctx, is_lhs, None)?;
                let expression_type = expression.kind.infer_data_type(ctx)?;

                match runtime_storage {
                    RuntimeStorageType::Score => {
                        if let Some(value) = expression_type.is_score_compatible(ctx)
                            && !value
                        {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::TypeIsNotScoreCompatible(
                                        expression_type,
                                    ),
                                ),
                            });
                        }
                    }
                    RuntimeStorageType::Data => {}
                }

                Some(())
            }
            ExpressionKind::Tuple(expressions) => expressions
                .iter()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs, None))
                .all_some(),
            ExpressionKind::Struct(name_span, name, generic_types, fields) => {
                let declaration = ctx.get_data_type(name);

                let declaration = match declaration {
                    None => {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: *name_span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnknownType(name.clone()),
                            ),
                        });
                    }
                    Some(Some(declaration)) => declaration,
                    Some(None) => return None,
                };

                let resolved_generic_types = generic_types
                    .iter()
                    .map(|generic_type| {
                        if generic_type.perform_semantic_analysis(None, ctx).is_none() {
                            None
                        } else {
                            generic_type.kind.resolve(ctx, None)
                        }
                    })
                    .collect::<Option<Vec<_>>>()?;

                declaration.perform_semantic_analysis(
                    ctx,
                    *name_span,
                    generic_types.len(),
                    &resolved_generic_types,
                )?;

                let defined_fields = declaration.get_struct_fields(ctx, &resolved_generic_types);

                if !declaration.resolve_is_struct(ctx, resolved_generic_types)? {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: *name_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotStruct(name.clone()),
                        ),
                    });
                }

                let defined_fields = defined_fields?;

                let mut has_error = false;

                for (field_name, field_value) in fields {
                    let Some(expected_field_type) = defined_fields.get(&field_name.snbt_string.1)
                    else {
                        has_error = true;

                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: field_name.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnexpectedField(
                                    field_name.snbt_string.1.clone(),
                                ),
                            ),
                        });

                        continue;
                    };

                    if field_value
                        .perform_semantic_analysis(ctx, is_lhs, Some(expected_field_type))
                        .is_none()
                    {
                        has_error = true;
                    }

                    let Some(actual_type) = field_value.kind.infer_data_type(ctx) else {
                        has_error = true;

                        continue;
                    };

                    if expected_field_type
                        .perform_equality_semantic_analysis(ctx, &actual_type, field_value)
                        .is_none()
                    {
                        has_error = true;
                    }
                }

                for defined_field in defined_fields.keys() {
                    if !fields
                        .keys()
                        .any(|field| field.snbt_string.1 == *defined_field)
                    {
                        has_error = true;

                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: self.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::MissingField(defined_field.clone()),
                            ),
                        });
                    }
                }

                if has_error { None } else { Some(()) }
            }
            ExpressionKind::Variable(name) => {
                if !ctx.variable_is_declared(name) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UndeclaredVariable(name.clone()),
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::Boolean(_)
            | ExpressionKind::Byte(_)
            | ExpressionKind::Short(_)
            | ExpressionKind::Integer(_)
            | ExpressionKind::Long(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::Double(_)
            | ExpressionKind::String(_)
            | ExpressionKind::Underscore
            | ExpressionKind::Unit => Some(()),
        }
    }

    #[must_use]
    pub fn get_dereferenced_place_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<PlaceType> {
        match &self.kind {
            ExpressionKind::PlayerScore(_) => {
                Some(PlaceTypeKind::Score(DataTypeKind::Integer).with_span(self.span))
            }
            ExpressionKind::Data(_, _) => {
                Some(PlaceTypeKind::Data(DataTypeKind::Inferred).with_span(self.span))
            }
            ExpressionKind::Boolean(_)
            | ExpressionKind::Byte(_)
            | ExpressionKind::Short(_)
            | ExpressionKind::Integer(_)
            | ExpressionKind::Long(_)
            | ExpressionKind::Float(_)
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
            | ExpressionKind::Struct(_, _, _, _) => None,
            ExpressionKind::Unary(operator, expression) => match operator {
                UnaryOperator::Reference => expression.get_place_type(supports_variable_type_scope),
                UnaryOperator::Dereference => todo!(),
                UnaryOperator::Negate | UnaryOperator::Invert => None,
            },
            ExpressionKind::Variable(name) => Some(
                supports_variable_type_scope
                    .get_variable(name)??
                    .as_dereferenced_place_type()
                    .ok()?
                    .with_span(self.span),
            ),
        }
    }

    pub fn get_place_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<PlaceType> {
        match &self.kind {
            ExpressionKind::Tuple(expressions) => Some(
                PlaceTypeKind::Tuple(
                    expressions
                        .iter()
                        .map(|expression| expression.get_place_type(supports_variable_type_scope))
                        .collect::<Option<_>>()?,
                    self.kind.infer_data_type(supports_variable_type_scope)?,
                )
                .with_span(self.span),
            ),
            ExpressionKind::PlayerScore(_) => {
                Some(PlaceTypeKind::Score(DataTypeKind::Integer).with_span(self.span))
            }
            ExpressionKind::Data(_, _) => {
                Some(PlaceTypeKind::Data(DataTypeKind::SNBT).with_span(self.span))
            }
            ExpressionKind::Unary(operator, expression) => match operator {
                UnaryOperator::Negate => todo!(),
                UnaryOperator::Reference => todo!(),
                UnaryOperator::Dereference => {
                    expression.get_dereferenced_place_type(supports_variable_type_scope)
                }
                UnaryOperator::Invert => todo!(),
            },
            ExpressionKind::Index(target, _) => {
                let target_place_type = target.get_place_type(supports_variable_type_scope)?;
                let target_data_type = target.kind.infer_data_type(supports_variable_type_scope)?;

                Some(
                    PlaceTypeKind::Index(Box::new(target_place_type), target_data_type)
                        .with_span(self.span),
                )
            }
            ExpressionKind::FieldAccess(target, field) => {
                let target_place_type = target.get_place_type(supports_variable_type_scope)?;
                let target_data_type = target.kind.infer_data_type(supports_variable_type_scope)?;

                Some(
                    PlaceTypeKind::FieldAccess(
                        Box::new(target_place_type),
                        target_data_type,
                        field.snbt_string.1.clone(),
                    )
                    .with_span(self.span),
                )
            }
            ExpressionKind::Boolean(_)
            | ExpressionKind::Byte(_)
            | ExpressionKind::Short(_)
            | ExpressionKind::Integer(_)
            | ExpressionKind::Long(_)
            | ExpressionKind::Float(_)
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
            | ExpressionKind::Unit => None,
            ExpressionKind::Underscore => Some(PlaceTypeKind::Underscore.with_span(self.span)),
            ExpressionKind::Variable(name) => {
                let variable_type = supports_variable_type_scope.get_variable(name)??;

                Some(PlaceTypeKind::Variable(variable_type).with_span(self.span))
            }
        }
    }

    pub fn resolve(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ResolvedExpression {
        match self.kind {
            ExpressionKind::Unary(unary_operator, expression) => match unary_operator {
                UnaryOperator::Negate => {
                    let expression = expression.resolve(datapack, ctx);

                    expression.negate(datapack, ctx)
                }
                UnaryOperator::Invert => {
                    let expression = expression.resolve(datapack, ctx);

                    expression.invert()
                }
                UnaryOperator::Reference => expression.resolve(datapack, ctx),
                UnaryOperator::Dereference => expression
                    .resolve(datapack, ctx)
                    .dereference(datapack, ctx)
                    .unwrap(),
            },
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                left.perform_arithmetic(datapack, ctx, operator, right)
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                left.perform_comparison(datapack, ctx, operator, right)
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                left.perform_logical_operation(datapack, ctx, operator, right)
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let value = value.resolve(datapack, ctx);

                let target_place = target.kind.as_place(datapack, ctx).unwrap();
                target_place.augmented_assign(datapack, ctx, operator, value);

                ResolvedExpression::Unit
            }
            ExpressionKind::Assignment(target, value) => {
                let value = value.resolve(datapack, ctx);

                let target_place = target.kind.as_place(datapack, ctx).unwrap();
                target_place.assign(datapack, ctx, value);

                ResolvedExpression::Unit
            }
            ExpressionKind::List(expressions) => ResolvedExpression::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            ExpressionKind::Compound(compound) => ResolvedExpression::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.resolve(datapack, ctx)))
                    .collect::<BTreeMap<_, _>>(),
            ),
            ExpressionKind::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                ResolvedExpression::PlayerScore(score)
            }
            ExpressionKind::Data(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                ResolvedExpression::Data(Box::new((target, path)))
            }
            ExpressionKind::Condition(inverted, condition) => {
                let condition = condition.compile(datapack, ctx);

                ResolvedExpression::Condition(inverted, Box::new(condition))
            }
            ExpressionKind::Command(command) => {
                let command = command.compile(datapack, ctx);

                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command))),
                        ),
                    )),
                );

                ResolvedExpression::PlayerScore(unique_score)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.resolve(datapack, ctx);
                let index = index.resolve(datapack, ctx);

                target.index(datapack, ctx, index).unwrap()
            }
            ExpressionKind::FieldAccess(target, field) => {
                let target = target.resolve(datapack, ctx);

                target.access_field(&field.snbt_string.1).unwrap()
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.resolve(datapack, ctx);
                let data_type = data_type.kind.resolve(datapack, None).unwrap();

                expression.cast_to(data_type)
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let expression = expression.resolve(datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        ResolvedExpression::PlayerScore(expression.as_score(datapack, ctx, true))
                    }
                    RuntimeStorageType::Data => {
                        let (unique_target, unique_path) = expression.as_data(datapack, ctx, true);

                        ResolvedExpression::Data(Box::new((unique_target, unique_path)))
                    }
                }
            }
            ExpressionKind::Tuple(expressions) => ResolvedExpression::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.resolve(datapack, ctx))
                    .collect(),
            ),
            ExpressionKind::Struct(_, name, generics, fields) => ResolvedExpression::Struct(
                name,
                generics
                    .into_iter()
                    .map(|generic| generic.kind.resolve(datapack, None).unwrap())
                    .collect(),
                fields
                    .into_iter()
                    .map(|(key, field)| (key.snbt_string.1, field.resolve(datapack, ctx)))
                    .collect(),
            ),
            ExpressionKind::Boolean(value) => ResolvedExpression::Boolean(value),
            ExpressionKind::Byte(value) => ResolvedExpression::Byte(value),
            ExpressionKind::Short(value) => ResolvedExpression::Short(value),
            ExpressionKind::Integer(value) => ResolvedExpression::Integer(value),
            ExpressionKind::Long(value) => ResolvedExpression::Long(value),
            ExpressionKind::Float(value) => ResolvedExpression::Float(value),
            ExpressionKind::Double(value) => ResolvedExpression::Double(value),
            ExpressionKind::String(value) => ResolvedExpression::String(value),
            ExpressionKind::Underscore => ResolvedExpression::Underscore,
            ExpressionKind::Unit => ResolvedExpression::Unit,
            ExpressionKind::Variable(name) => datapack.get_variable(&name).unwrap().1,
        }
    }
}
