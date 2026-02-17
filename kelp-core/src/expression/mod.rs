use std::collections::BTreeMap;

use minecraft_command_types_derive::HasMacro;
use parser_rs::parser_range::ParserRange;

use crate::{
    compile_context::CompileContext,
    data_type::{DataTypeKind, high::HighDataType},
    datapack::HighDatapack,
    expression::{
        constant::{ConstantExpression, ConstantExpressionKind},
        supports_variable_type_scope::SupportsVariableTypeScope,
    },
    high::{
        command::{HighCommand, execute::subcommand::r#if::HighExecuteIfSubcommand},
        data::HighDataTarget,
        nbt_path::HighNbtPath,
        player_score::{GeneratedPlayerScore, HighPlayerScore},
        snbt_string::HighSNBTString,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    place::{Place, PlaceType, PlaceTypeKind},
    runtime_storage_type::RuntimeStorageType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionUnitIterExt,
};

pub mod constant;
pub mod literal;
pub mod supports_variable_type_scope;
pub mod utils;

pub type ExpressionCompoundKind = BTreeMap<HighSNBTString, Expression>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ExpressionKind {
    Constant(ConstantExpression),
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
    Struct(
        #[has_macro(ignore)] ParserRange,
        String,
        Vec<HighDataType>,
        BTreeMap<HighSNBTString, Expression>,
    ),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    pub fn is_lvalue(&self, ctx: &mut SemanticAnalysisContext) -> Option<bool> {
        Some(match self {
            ExpressionKind::Constant(expression) => expression.kind.is_lvalue(),
            ExpressionKind::Unary(UnaryOperator::Dereference, _) => true,
            ExpressionKind::PlayerScore(_) => true,
            ExpressionKind::Data(_, _) => true,
            ExpressionKind::Index(_, _) => true,
            ExpressionKind::FieldAccess(_, _) => true,
            ExpressionKind::AsCast(_, high_data_type) => {
                high_data_type.kind.resolve(ctx, None)?.is_lvalue()
            }
            ExpressionKind::ToCast(_, _) => true,
            _ => false,
        })
    }

    pub fn can_be_dereferenced(&self) -> bool {
        match self {
            ExpressionKind::Constant(expression) => expression.kind.can_be_dereferenced(),
            ExpressionKind::Unary(UnaryOperator::Reference, _) => true,
            ExpressionKind::Unary(UnaryOperator::Dereference, _) => true,
            _ => false,
        }
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            ExpressionKind::Constant(constant_expression) => constant_expression
                .kind
                .infer_data_type(supports_variable_type_scope)?,
            ExpressionKind::Unary(operator, expression) => {
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
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left_type = left.kind.infer_data_type(supports_variable_type_scope)?;
                let right_type = right.kind.infer_data_type(supports_variable_type_scope)?;

                left_type.get_arithmetic_result(operator, &right_type)?
            }
            ExpressionKind::Comparison(_, _, _) | ExpressionKind::Logical(_, _, _) => {
                DataTypeKind::Boolean
            }
            ExpressionKind::Assignment(_, _) | ExpressionKind::AugmentedAssignment(_, _, _) => {
                DataTypeKind::Unit
            }
            ExpressionKind::List(list) => {
                let data_type = if let Some(first) = list.first() {
                    first.kind.infer_data_type(supports_variable_type_scope)?
                } else {
                    DataTypeKind::SNBT
                };

                DataTypeKind::List(Box::new(data_type))
            }
            ExpressionKind::Compound(compound) => DataTypeKind::TypedCompound(
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
            ExpressionKind::PlayerScore(_) => DataTypeKind::Score,
            ExpressionKind::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::SNBT)),
            ExpressionKind::Condition(_, _) => DataTypeKind::Byte,
            ExpressionKind::Command(_) => DataTypeKind::Integer,
            ExpressionKind::Index(target, _) => target
                .kind
                .infer_data_type(supports_variable_type_scope)?
                .get_index_result()?,
            ExpressionKind::FieldAccess(target, field) => target
                .kind
                .infer_data_type(supports_variable_type_scope)?
                .get_field_result(supports_variable_type_scope, &field.snbt_string.1)?,
            ExpressionKind::AsCast(_, data_type) => {
                data_type.kind.resolve(supports_variable_type_scope, None)?
            }
            ExpressionKind::ToCast(expression, storage_type) => match storage_type {
                RuntimeStorageType::Score => DataTypeKind::Score,
                RuntimeStorageType::Data => {
                    let expression_type = expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?;

                    DataTypeKind::Data(Box::new(expression_type.to_data()))
                }
            },
            ExpressionKind::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::Struct(_, name, generic_types, _) => {
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
}

impl ExpressionKind {
    pub fn into_dummy_expression(self) -> Expression {
        Expression {
            span: ParserRange::default(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub struct Expression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn as_place(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Place {
        match self.kind {
            ExpressionKind::Constant(expression) => expression.kind.as_place(),
            ExpressionKind::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                Place::Score(score)
            }
            ExpressionKind::Data(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Place::Data(target, path)
            }
            ExpressionKind::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.as_place(datapack, ctx))
                    .collect(),
            ),
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                Place::Dereference(expression)
            }
            ExpressionKind::FieldAccess(expression, field) => {
                let expression = expression.resolve(datapack, ctx);

                Place::Field(Box::new(expression), field.snbt_string)
            }
            _ => unreachable!("This expression is not a place {:?}", self),
        }
    }

    pub fn dereference(self, datapack: &mut HighDatapack) -> Expression {
        match self.kind {
            ExpressionKind::Constant(expression) => match expression.kind {
                ConstantExpressionKind::Variable(name) => datapack
                    .get_variable(&name)
                    .unwrap()
                    .1
                    .into_constant_expression()
                    .dereference(datapack),
                ConstantExpressionKind::Reference(expression) => {
                    expression.into_constant_expression()
                }
                kind @ ConstantExpressionKind::PlayerScore(_) => Expression {
                    span: self.span,
                    kind: ExpressionKind::Constant(ConstantExpression {
                        span: self.span,
                        kind,
                    }),
                },
                kind @ ConstantExpressionKind::Data(_, _) => Expression {
                    span: self.span,
                    kind: ExpressionKind::Constant(ConstantExpression {
                        span: self.span,
                        kind,
                    }),
                },
                expression => {
                    unreachable!("This expression cannot be dereferenced {:?}", expression)
                }
            },
            ExpressionKind::PlayerScore(_) => self,
            ExpressionKind::Data(_, _) => self,
            ExpressionKind::Unary(UnaryOperator::Reference, expression) => *expression,
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                expression.dereference(datapack).dereference(datapack)
            }
            _ => unreachable!("This expression cannot be dereferenced {:?}", self),
        }
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
        expected_type: Option<&DataTypeKind>,
    ) -> Option<()> {
        match &self.kind {
            ExpressionKind::Constant(constant_expression) => {
                constant_expression.perform_semantic_analysis(ctx, is_lhs)
            }
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
                        if !expression.kind.is_lvalue(ctx)? {
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

                let result_type = left_type.get_arithmetic_result(operator, &right_type);

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

                if !left_type.can_perform_comparison(operator, &right_type) {
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
            ExpressionKind::Logical(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs, None);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs, None);

                let left_type = left.kind.infer_data_type(ctx)?;
                let right_type = right.kind.infer_data_type(ctx)?;

                if !left_type.can_perform_logical_comparison(operator, &right_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformLogicalOperation {
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
                    *value.clone(),
                    &value_data_type,
                )?;

                Some(())
            }
            ExpressionKind::Assignment(target, value) => {
                target.perform_semantic_analysis(ctx, true, None)?;

                let target_type = target.kind.infer_data_type(ctx)?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                let value_result = value.perform_semantic_analysis(ctx, false, Some(&target_type));
                value_result?;

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
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs, None);

                expression_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;

                match runtime_storage {
                    RuntimeStorageType::Score => {
                        if !expression_type.is_score_value() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeAssignedToScore(expression_type),
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
                    match defined_fields.get(&field_name.snbt_string.1) {
                        Some(expected_field_type) => {
                            if field_value
                                .perform_semantic_analysis(ctx, is_lhs, Some(expected_field_type))
                                .is_none()
                            {
                                has_error = true;
                            }

                            if let Some(actual_type) = field_value.kind.infer_data_type(ctx) {
                                if expected_field_type
                                    .perform_equality_semantic_analysis(
                                        ctx,
                                        &actual_type,
                                        field_value,
                                    )
                                    .is_none()
                                {
                                    has_error = true;
                                }
                            } else {
                                has_error = true;
                            }
                        }
                        None => {
                            has_error = true;

                            ctx.add_info::<()>(SemanticAnalysisInfo {
                                span: field_name.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnexpectedField(
                                        field_name.snbt_string.1.clone(),
                                    ),
                                ),
                            });
                        }
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
        }
    }

    pub fn get_place_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<PlaceType> {
        Some(
            (match &self.kind {
                ExpressionKind::Tuple(expressions) => PlaceTypeKind::Tuple(
                    expressions
                        .iter()
                        .map(|expression| expression.get_place_type(supports_variable_type_scope))
                        .collect::<Option<_>>()?,
                    self.kind.infer_data_type(supports_variable_type_scope)?,
                ),
                ExpressionKind::Constant(expression) => {
                    return expression.get_place_type(supports_variable_type_scope);
                }
                ExpressionKind::PlayerScore(_) => PlaceTypeKind::Score,
                ExpressionKind::Data(_, _) => PlaceTypeKind::Data(DataTypeKind::SNBT),
                ExpressionKind::Unary(UnaryOperator::Dereference, _) => self
                    .kind
                    .infer_data_type(supports_variable_type_scope)?
                    .as_place_type()
                    .ok()?,
                ExpressionKind::AsCast(expression, data_type) => {
                    let resolved_data_type =
                        data_type.kind.resolve(supports_variable_type_scope, None)?;

                    if !expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?
                        .can_cast_to(&resolved_data_type)
                    {
                        return None;
                    }

                    resolved_data_type.as_place_type().ok()?
                }
                ExpressionKind::FieldAccess(_, _) => self
                    .kind
                    .infer_data_type(supports_variable_type_scope)?
                    .as_place_type()
                    .ok()?,
                _ => return None,
            })
            .with_span(self.span),
        )
    }

    pub fn resolve(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpressionKind {
        match self.kind {
            ExpressionKind::Constant(expression) => expression.kind.resolve(datapack),
            ExpressionKind::Unary(UnaryOperator::Reference, expression) => {
                ConstantExpressionKind::Reference(Box::new(
                    expression
                        .resolve(datapack, ctx)
                        .into_dummy_constant_expression(),
                ))
            }
            _ => self.resolve_partial(datapack, ctx),
        }
    }

    pub fn resolve_partial(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpressionKind {
        match self.kind {
            ExpressionKind::Constant(expression) => expression.kind,
            ExpressionKind::Unary(unary_operator, expression) => match unary_operator {
                UnaryOperator::Negate => {
                    let expression = expression.resolve(datapack, ctx);

                    expression.negate(datapack, ctx)
                }
                UnaryOperator::Invert => {
                    let expression = expression.resolve(datapack, ctx);

                    expression.invert()
                }
                UnaryOperator::Reference => ConstantExpressionKind::Reference(Box::new(
                    expression
                        .resolve_partial(datapack, ctx)
                        .into_dummy_constant_expression(),
                )),
                UnaryOperator::Dereference => {
                    expression.resolve(datapack, ctx).dereference(datapack, ctx)
                }
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

                target
                    .as_place(datapack, ctx)
                    .augmented_assign(datapack, ctx, operator, value);

                ConstantExpressionKind::Unit
            }
            ExpressionKind::Assignment(target, value) => {
                let value = value.resolve(datapack, ctx);

                target.as_place(datapack, ctx).assign(datapack, ctx, value);

                ConstantExpressionKind::Unit
            }
            ExpressionKind::List(expressions) => ConstantExpressionKind::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.resolve(datapack, ctx).into_dummy_constant_expression())
                    .collect::<Vec<_>>(),
            ),
            ExpressionKind::Compound(compound) => ConstantExpressionKind::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        (
                            key,
                            value
                                .resolve(datapack, ctx)
                                .into_dummy_constant_expression(),
                        )
                    })
                    .collect::<BTreeMap<_, _>>(),
            ),
            ExpressionKind::PlayerScore(score) => {
                let score = score.clone().compile(datapack, ctx);

                ConstantExpressionKind::PlayerScore(score)
            }
            ExpressionKind::Data(target, path) => {
                let target = target.clone().compile(datapack, ctx);
                let path = path.clone().compile(datapack, ctx);

                ConstantExpressionKind::Data(target, path)
            }
            ExpressionKind::Condition(inverted, condition) => {
                // TODO why optional?
                let condition = condition.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Condition(inverted, condition)
            }
            ExpressionKind::Command(command) => {
                // TODO why optional?
                let command = command.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Command(command)
            }
            ExpressionKind::Index(target, index) => {
                let target = target.resolve(datapack, ctx);
                let index = index.resolve(datapack, ctx);

                target.index(index)
            }
            ExpressionKind::FieldAccess(target, field) => {
                let target = target.resolve(datapack, ctx);

                target.access_field(field.snbt_string.1)
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.resolve(datapack, ctx);
                let data_type = data_type.kind.resolve(datapack, None).unwrap();

                expression.cast_to(datapack, data_type)
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let expression = expression.resolve(datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        let score = expression.as_score(datapack, ctx, true);

                        ConstantExpressionKind::PlayerScore(score)
                    }
                    RuntimeStorageType::Data => {
                        let (unique_target, unique_path) = expression.as_data_force(datapack, ctx);

                        ConstantExpressionKind::Data(unique_target, unique_path)
                    }
                }
            }
            ExpressionKind::Tuple(expressions) => ConstantExpressionKind::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| {
                        expression
                            .resolve(datapack, ctx)
                            .into_dummy_constant_expression()
                    })
                    .collect(),
            ),
            ExpressionKind::Struct(_, name, generics, fields) => ConstantExpressionKind::Struct(
                name,
                generics
                    .into_iter()
                    .map(|generic| generic.kind.resolve(datapack, None).unwrap())
                    .collect(),
                fields
                    .into_iter()
                    .map(|(key, field)| {
                        (
                            key.snbt_string.1,
                            field
                                .resolve(datapack, ctx)
                                .into_dummy_constant_expression(),
                        )
                    })
                    .collect(),
            ),
        }
    }

    pub fn resolve_into_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
    ) {
        match self.kind {
            ExpressionKind::Arithmetic(left, operator, right) => {
                left.resolve_into_score(datapack, ctx, target.clone());
                right
                    .resolve(datapack, ctx)
                    .operate_on_score(datapack, ctx, target, operator);
            }
            _ => {
                self.resolve(datapack, ctx)
                    .assign_to_score(datapack, ctx, target);
            }
        }
    }
}
