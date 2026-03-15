use std::collections::BTreeMap;

use crate::compile_context::LoopInfo;
use crate::high::data_type::DataType;
use crate::high::item::Item;
use crate::span::Span;
use crate::trait_ext::CollectOptionAllIterExt;
use crate::{
    compile_context::CompileContext,
    high::expression::Expression,
    middle::statement::Statement as MiddleStatement,
    pattern::Pattern,
    semantic_analysis_context::{
        Scope, SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
};
use minecraft_command_types::range::IntegerRange;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Let(Option<DataType>, Pattern, Expression),
    While(Expression, Box<Statement>),
    Loop(Box<Statement>),
    Match(Expression, BTreeMap<IntegerRange, Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    ForIn(bool, String, Expression, Box<Statement>),
    Block(Vec<Statement>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct ControlFlow {
    pub kind: ControlFlowKind,
    pub loop_info: LoopInfo,
}

#[derive(Debug, Clone, Copy)]
pub enum ControlFlowKind {
    Break,
    Continue,
}

impl StatementKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Statement {
        Statement { span, kind: self }
    }

    #[must_use]
    pub fn get_control_flow_kind(&self) -> Option<ControlFlowKind> {
        match self {
            Self::Expression(_)
            | Self::Let(_, _, _)
            | Self::Append(_, _)
            | Self::Remove(_)
            | Self::Item(_) => None,
            Self::While(_, statement) | Self::Loop(statement) | Self::ForIn(_, _, _, statement) => {
                statement.kind.get_control_flow_kind()
            }
            Self::Match(_, _) => todo!(),
            Self::If(_, statement, else_statement) => {
                statement.kind.get_control_flow_kind().or_else(|| {
                    else_statement
                        .as_ref()
                        .and_then(|statement| statement.kind.get_control_flow_kind())
                })
            }
            Self::Block(statements) => statements
                .iter()
                .find_map(|statement| statement.kind.get_control_flow_kind()),
            Self::Break => Some(ControlFlowKind::Break),
            Self::Continue => Some(ControlFlowKind::Continue),
        }
    }

    #[must_use]
    pub fn get_control_flow(&self, ctx: &mut CompileContext) -> Option<ControlFlow> {
        let loop_info = ctx.loop_info.as_ref()?.clone();

        Some(ControlFlow {
            kind: self.get_control_flow_kind()?,
            loop_info,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

impl Statement {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleStatement> {
        Some(match self.kind {
            StatementKind::Expression(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleStatement::Expression(expression)
            }
            StatementKind::Let(explicit_type, pattern, value) => {
                let explicit_type = match explicit_type {
                    Some(explicit_type) => {
                        let Some(explicit_type) =
                            explicit_type.perform_semantic_analysis(None, ctx)
                        else {
                            pattern.kind.destructure_unknown(ctx);

                            return None;
                        };

                        Some(explicit_type)
                    }
                    None => None,
                };

                let (value_span, value) = value.perform_semantic_analysis(ctx, is_lhs)?;

                let variable_type = explicit_type.unwrap_or_else(|| value.data_type.clone());

                if variable_type
                    .clone()
                    .destructure(ctx, value_span, &pattern)
                    .is_some()
                {
                    if !value.data_type.equals(&variable_type) {
                        ctx.add_error(
                            value_span,
                            SemanticAnalysisError::MismatchedTypes {
                                expected: variable_type.clone(),
                                actual: value.data_type.clone(),
                            },
                        );
                    }
                } else {
                    pattern.kind.destructure_unknown(ctx);
                }

                MiddleStatement::Let(variable_type, pattern, value)
            }
            StatementKind::While(condition, body) => {
                let condition = condition.perform_semantic_analysis(ctx, is_lhs);

                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx, is_lhs);
                ctx.loop_depth -= 1;

                let (condition_span, condition) = condition?;

                if !condition.data_type.is_condition() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: condition_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotCondition(condition.data_type),
                        ),
                    });
                }

                let body = body?;

                MiddleStatement::While(condition, Box::new(body))
            }
            StatementKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx, is_lhs);
                ctx.loop_depth -= 1;

                let body = body?;

                MiddleStatement::Loop(Box::new(body))
            }
            StatementKind::Match(_, _) => todo!(),
            StatementKind::If(condition, statement, else_statement) => {
                let condition = condition.perform_semantic_analysis(ctx, is_lhs);
                let statement = statement.perform_semantic_analysis(ctx, is_lhs);
                let else_statement = match else_statement {
                    Some(else_statement) => {
                        Some(else_statement.perform_semantic_analysis(ctx, is_lhs)?)
                    }
                    None => None,
                };

                let (expression_span, condition) = condition?;

                if !condition.data_type.is_condition() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsNotCondition(condition.data_type),
                        ),
                    });
                }

                let statement = statement?;

                MiddleStatement::If(condition, Box::new(statement), else_statement.map(Box::new))
            }
            StatementKind::ForIn(is_reversed, name, iterable, statement) => {
                let (expression_span, iterable) =
                    iterable.perform_semantic_analysis(ctx, is_lhs)?;

                let statement = statement.perform_semantic_analysis(ctx, is_lhs);

                let Some(iterable_type) = iterable.data_type.get_iterable_type() else {
                    ctx.declare_variable_unknown(&name);

                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression_span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotIterateType(iterable.data_type),
                        ),
                    });
                };

                ctx.declare_variable_known(&name, iterable_type);

                let statement = statement?;

                // TODO: Reorder semantic analysis

                MiddleStatement::ForIn(is_reversed, name, iterable, Box::new(statement))
            }
            StatementKind::Block(statements) => {
                ctx.scopes.push(Scope::default());

                let statements = statements
                    .into_iter()
                    .map(|statement| statement.perform_semantic_analysis(ctx, is_lhs))
                    .collect_option_all();

                ctx.scopes.pop();

                let statements = statements?;

                MiddleStatement::Block(statements)
            }
            StatementKind::Append(target, value) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let value = value.perform_semantic_analysis(ctx, is_lhs);

                let (_, target) = target?;
                let (_, value) = value?;

                MiddleStatement::Append(target, Box::new(value))
            }
            StatementKind::Remove(target) => {
                let (_, target) = target.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleStatement::Remove(target)
            }
            StatementKind::Break => {
                if ctx.loop_depth == 0 {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Break),
                        ),
                    });
                }

                MiddleStatement::Break
            }
            StatementKind::Continue => {
                if ctx.loop_depth == 0 {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Continue),
                        ),
                    });
                }

                MiddleStatement::Continue
            }
            StatementKind::Item(item) => {
                let item = item.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleStatement::Item(Box::new(item))
            }
        })
    }

    #[must_use]
    pub const fn new(span: Span, kind: StatementKind) -> Self {
        Self { span, kind }
    }
}
