use std::collections::HashMap;

use crate::high::data_type::unresolved::UnresolvedDataType;
use crate::high::item::Item;
use crate::high::pattern::Pattern;
use crate::high::semantic_analysis_context::SemanticAnalysisContext;
use crate::high::semantic_analysis_context::info::error::SemanticAnalysisError;
use crate::low::statement::ControlFlowKind;
use crate::span::Span;
use crate::{high::expression::Expression, low::statement::Statement as MiddleStatement};
use minecraft_command_types::range::IntegerRange;

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Let(Option<UnresolvedDataType>, Pattern, Expression),
    Match(Expression, HashMap<IntegerRange, Box<Statement>>),
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
            StatementKind::Match(_, _) => todo!(),
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
