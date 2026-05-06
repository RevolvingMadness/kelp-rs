use crate::high::data_type::DataType;
use crate::high::expression::Expression;
use crate::high::item::Item;
use crate::high::pattern::Pattern;
use crate::high::semantic_analysis::SemanticAnalysisContext;
use crate::high::semantic_analysis::info::error::SemanticAnalysisError;
use crate::low::statement::{LoopControlFlowKind, UnresolvedStatement};
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Let(Option<DataType>, Pattern, Expression),
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
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<UnresolvedStatement> {
        Some(match self.kind {
            StatementKind::Expression(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Expression(expression)
            }
            StatementKind::Let(explicit_type, pattern, value) => {
                let explicit_type =
                    explicit_type.map(|explicit_type| explicit_type.resolve_partially(None, ctx));

                let Some((_, value)) = value.perform_semantic_analysis(ctx) else {
                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                let variable_type = explicit_type.unwrap_or_else(|| value.data_type.clone());

                let pattern = pattern.perform_semantic_analysis(ctx, &variable_type)?;

                UnresolvedStatement::Let(variable_type, pattern, Box::new(value))
            }
            StatementKind::Append(target, value) => {
                let target = target.perform_semantic_analysis(ctx);
                let value = value.perform_semantic_analysis(ctx);

                let (_, target) = target?;
                let (_, value) = value?;

                UnresolvedStatement::Append(target, Box::new(value))
            }
            StatementKind::Remove(target) => {
                let (_, target) = target.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Remove(target)
            }
            StatementKind::Break => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Break),
                    );
                }

                UnresolvedStatement::Break
            }
            StatementKind::Continue => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Continue),
                    );
                }

                UnresolvedStatement::Continue
            }
            StatementKind::Item(item) => {
                let item = item.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Item(Box::new(item))
            }
        })
    }

    #[must_use]
    pub const fn new(span: Span, kind: StatementKind) -> Self {
        Self { span, kind }
    }
}
