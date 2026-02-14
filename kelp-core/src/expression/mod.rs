use std::collections::BTreeMap;

use minecraft_command_types::command::{
    Command,
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
};
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
    trait_ext::OptionIterExt,
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
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    pub fn is_lvalue(&self) -> bool {
        match self {
            ExpressionKind::Constant(expression) => expression.kind.is_lvalue(),
            ExpressionKind::Unary(UnaryOperator::Dereference, _) => true,
            ExpressionKind::PlayerScore(_) => true,
            ExpressionKind::Data(_, _) => true,
            ExpressionKind::Index(_, _) => true,
            ExpressionKind::FieldAccess(_, _) => true,
            ExpressionKind::AsCast(_, high_data_type) => high_data_type.kind.resolve().is_lvalue(),
            ExpressionKind::ToCast(_, _) => true,
            _ => false,
        }
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
                    UnaryOperator::Negate => expression_type
                        .get_negated_result()
                        .expect("Expression cannot be negated"),
                    UnaryOperator::Reference => DataTypeKind::Reference(Box::new(expression_type)),
                    UnaryOperator::Dereference => expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?
                        .dereference()?,
                    UnaryOperator::Invert => expression_type
                        .get_inverted_result()
                        .expect("Expression cannot be inverted"),
                }
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left_type = left.kind.infer_data_type(supports_variable_type_scope)?;
                let right_type = right.kind.infer_data_type(supports_variable_type_scope)?;

                left_type
                    .get_arithmetic_result(operator, &right_type)
                    .expect("Cannot perform arithmetic")
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
                .get_index_result()
                .expect("Expression cannot be indexed"),
            ExpressionKind::FieldAccess(target, field) => target
                .kind
                .infer_data_type(supports_variable_type_scope)?
                .get_field_result(&field.snbt_string)
                .expect("Expression does not have the specified field"),
            ExpressionKind::AsCast(_, data_type) => data_type.kind.resolve(),
            ExpressionKind::ToCast(expression, storage_type) => match storage_type {
                RuntimeStorageType::Score => DataTypeKind::Score,
                RuntimeStorageType::Data => DataTypeKind::Data(Box::new(
                    expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?,
                )),
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
    pub fn as_place(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Place> {
        Some(match self.kind {
            ExpressionKind::Constant(expression) => return expression.kind.as_place(),
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
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                Place::Dereference(expression)
            }
            _ => return None,
        })
    }

    pub fn dereference(self, datapack: &mut HighDatapack) -> Option<Expression> {
        Some(match self.kind {
            ExpressionKind::Constant(expression) => match expression.kind {
                ConstantExpressionKind::Variable(name) => datapack
                    .get_variable(&name)
                    .unwrap()
                    .1
                    .into_constant_expression()
                    .dereference(datapack)?,
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
                _ => return None,
            },
            ExpressionKind::PlayerScore(_) => self,
            ExpressionKind::Data(_, _) => self,
            ExpressionKind::Unary(UnaryOperator::Reference, expression) => *expression,
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                expression.dereference(datapack)?.dereference(datapack)?
            }
            _ => return None,
        })
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            ExpressionKind::Constant(constant_expression) => {
                constant_expression.perform_semantic_analysis(ctx, is_lhs)
            }
            ExpressionKind::Unary(operator, expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)?;

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
                        if !expression.kind.is_lvalue() {
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
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

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

                Some(())
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

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

                Some(())
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

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

                Some(())
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let target_result = target.perform_semantic_analysis(ctx, is_lhs);
                let value_result = value.perform_semantic_analysis(ctx, is_lhs);

                target_result?;

                target.kind.infer_data_type(ctx)?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                value_result?;

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
                let target_result = target.perform_semantic_analysis(ctx, true);
                let value_result = value.perform_semantic_analysis(ctx, false);

                target_result?;

                target.kind.infer_data_type(ctx)?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                value_result?;

                let value_data_type = value.kind.infer_data_type(ctx)?;

                place.perform_assignment_semantic_analysis(
                    ctx,
                    *value.clone(),
                    &value_data_type,
                )?;

                Some(())
            }
            ExpressionKind::List(expressions) => expressions
                .iter()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            ExpressionKind::Compound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
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
                let target_result = target.perform_semantic_analysis(ctx, is_lhs);
                let index_result = index.perform_semantic_analysis(ctx, is_lhs);

                target_result?;

                let target_type = target.kind.infer_data_type(ctx)?;

                if !target_type.can_be_indexed() {
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
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

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

                if !expression_type.has_field(field) {
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
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);
                let data_type_result = data_type.perform_semantic_analysis(ctx);

                expression_result?;
                data_type_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;
                let data_type = data_type.kind.resolve();

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
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

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
                    RuntimeStorageType::Data => {
                        if !expression_type.can_be_assigned_to_data() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: expression.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CannotBeAssignedToData(expression_type),
                                ),
                            });
                        }
                    }
                }

                Some(())
            }
            ExpressionKind::Tuple(expressions) => expressions
                .iter()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
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
                ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                    PlaceTypeKind::Dereference(Box::new(
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)?,
                    ))
                }
                ExpressionKind::AsCast(expression, data_type) => {
                    let resolved_data_type = data_type.kind.resolve();

                    if !expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?
                        .can_cast_to(&resolved_data_type)
                    {
                        return None;
                    }

                    resolved_data_type.as_place_type()?
                }
                _ => return None,
            })
            .with_span(self.span),
        )
    }
    pub fn resolve_force(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpressionKind {
        let resolved = self.resolve(datapack, ctx);

        match resolved {
            ConstantExpressionKind::PlayerScore(ref score) => {
                if score.is_generated {
                    return resolved;
                }

                let unique_score = datapack.get_unique_score();

                resolved.assign_to_score(datapack, ctx, unique_score.clone());

                ConstantExpressionKind::PlayerScore(unique_score)
            }
            ConstantExpressionKind::Data(ref target, _) => {
                if target.is_generated {
                    return resolved;
                }

                let (unique_target, unique_path) = datapack.get_unique_data();

                resolved.assign_to_data(datapack, ctx, unique_target.clone(), unique_path.clone());

                ConstantExpressionKind::Data(unique_target, unique_path)
            }
            _ => resolved,
        }
    }

    pub fn resolve(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpressionKind {
        match self.kind {
            ExpressionKind::Constant(expression) => expression.resolve(datapack),
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
                    let resolved_expression = expression.resolve(datapack, ctx);
                    let new_score = resolved_expression.as_score(datapack, ctx, true);
                    let constant_score = datapack.get_constant_score(-1);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                new_score.score.clone(),
                                ScoreOperationOperator::Multiply,
                                constant_score.score,
                            ),
                        )),
                    );

                    ConstantExpressionKind::PlayerScore(new_score)
                }
                UnaryOperator::Invert => {
                    let expression = expression.resolve(datapack, ctx);

                    expression.invert().unwrap()
                }
                UnaryOperator::Reference => ConstantExpressionKind::Reference(Box::new(
                    expression
                        .resolve_partial(datapack, ctx)
                        .into_dummy_constant_expression(),
                )),
                UnaryOperator::Dereference => {
                    expression.resolve(datapack, ctx).dereference(datapack)
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
                    .unwrap()
                    .augmented_assign(datapack, ctx, operator, value);

                ConstantExpressionKind::Unit
            }
            ExpressionKind::Assignment(target, value) => {
                let value = value.resolve(datapack, ctx);

                target.as_place(datapack, ctx).unwrap().assign(
                    datapack,
                    ctx,
                    value.into_dummy_constant_expression(),
                );

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

                target.access_field(field.snbt_string)
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.resolve(datapack, ctx);
                let data_type = data_type.kind.resolve();

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
