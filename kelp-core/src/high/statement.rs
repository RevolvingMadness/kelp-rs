use std::collections::HashMap;

use crate::high::data_type::unresolved::UnresolvedDataType;
use crate::high::item::Item;
use crate::high::pattern::Pattern;
use crate::high::semantic_analysis_context::SemanticAnalysisContext;
use crate::high::semantic_analysis_context::info::error::SemanticAnalysisError;
use crate::middle::statement::ControlFlowKind;
use crate::span::Span;
use crate::trait_ext::CollectOptionAllIterExt;
use crate::{high::expression::Expression, middle::statement::Statement as MiddleStatement};
use minecraft_command_types::range::IntegerRange;

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Let(Option<UnresolvedDataType>, Pattern, Expression),
    While(Expression, Box<Statement>),
    Loop(Box<Statement>),
    Match(Expression, HashMap<IntegerRange, Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    For(bool, Pattern, Expression, Box<Statement>),
    Block(Vec<Statement>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
}

impl StatementKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Statement {
        Statement { span, kind: self }
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

impl Statement {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleStatement> {
        Some(match self.kind {
            StatementKind::Expression(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleStatement::Expression(expression)
            }
            StatementKind::Let(explicit_type, pattern, value) => {
                let explicit_type = match explicit_type {
                    Some(explicit_type) => {
                        let Some(explicit_type) = explicit_type.resolve_fully(ctx) else {
                            pattern.kind.destructure_unknown(ctx);

                            return None;
                        };

                        Some(explicit_type)
                    }
                    None => None,
                };

                let Some((_, value)) = value.perform_semantic_analysis(ctx) else {
                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                let variable_type = explicit_type.unwrap_or_else(|| value.data_type.clone());

                let pattern = pattern.perform_semantic_analysis(ctx, variable_type.clone())?;

                MiddleStatement::Let(variable_type, pattern, value)
            }
            StatementKind::While(condition, body) => {
                let condition = condition.perform_semantic_analysis(ctx);

                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (condition_span, condition) = condition?;

                if !condition.data_type.is_condition() {
                    return ctx.add_error(
                        condition_span,
                        SemanticAnalysisError::TypeIsNotCondition(condition.data_type),
                    );
                }

                let body = body?;

                MiddleStatement::While(condition, Box::new(body))
            }
            StatementKind::Loop(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let body = body?;

                MiddleStatement::Loop(Box::new(body))
            }
            StatementKind::Match(_, _) => todo!(),
            StatementKind::If(condition, statement, else_statement) => {
                let condition = condition.perform_semantic_analysis(ctx);
                let statement = statement.perform_semantic_analysis(ctx);
                let else_statement = match else_statement {
                    Some(else_statement) => Some(else_statement.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let (expression_span, condition) = condition?;

                if !condition.data_type.is_condition() {
                    return ctx.add_error(
                        expression_span,
                        SemanticAnalysisError::TypeIsNotCondition(condition.data_type),
                    );
                }

                let statement = statement?;

                MiddleStatement::If(condition, Box::new(statement), else_statement.map(Box::new))
            }
            StatementKind::For(is_reversed, pattern, iterable, statement) => {
                let (expression_span, iterable) = iterable.perform_semantic_analysis(ctx)?;

                let Some(iterable_type) = iterable.data_type.get_iterable_type() else {
                    pattern.kind.destructure_unknown(ctx);

                    return ctx.add_error(
                        expression_span,
                        SemanticAnalysisError::CannotIterateType(iterable.data_type),
                    );
                };

                ctx.enter_scope();

                let Some(pattern) = pattern.perform_semantic_analysis(ctx, iterable_type) else {
                    ctx.exit_scope();

                    return None;
                };

                let Some(statement) = statement.perform_semantic_analysis(ctx) else {
                    ctx.exit_scope();

                    return None;
                };

                ctx.exit_scope();

                // TODO: Reorder semantic analysis

                MiddleStatement::For(is_reversed, pattern, iterable, Box::new(statement))
            }
            StatementKind::Block(statements) => {
                ctx.enter_scope();

                let statements = statements
                    .into_iter()
                    .map(|statement| statement.perform_semantic_analysis(ctx))
                    .collect_option_all();

                ctx.exit_scope();

                let statements = statements?;

                MiddleStatement::Block(statements)
            }
            StatementKind::Append(target, value) => {
                let target = target.perform_semantic_analysis(ctx);
                let value = value.perform_semantic_analysis(ctx);

                let (_, target) = target?;
                let (_, value) = value?;

                MiddleStatement::Append(target, Box::new(value))
            }
            StatementKind::Remove(target) => {
                let (_, target) = target.perform_semantic_analysis(ctx)?;

                MiddleStatement::Remove(target)
            }
            StatementKind::Break => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Break),
                    );
                }

                MiddleStatement::Break
            }
            StatementKind::Continue => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(ControlFlowKind::Continue),
                    );
                }

                MiddleStatement::Continue
            }
            StatementKind::Item(item) => {
                let item = item.perform_semantic_analysis(ctx)?;

                MiddleStatement::Item(Box::new(item))
            }
        })
    }

    #[must_use]
    pub const fn new(span: Span, kind: StatementKind) -> Self {
        Self { span, kind }
    }
}
