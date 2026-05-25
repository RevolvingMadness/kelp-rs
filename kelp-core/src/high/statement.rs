use la_arena::Idx;

use crate::ast_allocator::high::HighAstAllocator;
use crate::ast_allocator::low::LowAstAllocator;
use crate::high::data_type::DataType;
use crate::high::expression::Expression;
use crate::high::item::Item;
use crate::high::pattern::Pattern;
use crate::high::semantic_analysis::SemanticAnalysisContext;
use crate::high::semantic_analysis::info::error::SemanticAnalysisError;
use crate::low::statement::{LoopControlFlowKind, UnresolvedStatement};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Idx<Expression>),
    Let(Option<DataType>, Idx<Pattern>, Idx<Expression>),
    Append(Idx<Expression>, Idx<Expression>),
    Remove(Idx<Expression>),
    Item(Idx<Item>),
    Break,
    Continue,
}

impl Statement {
    #[must_use]
    pub fn perform_semantic_analysis(
        id: Idx<Self>,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<UnresolvedStatement>> {
        Some(match high_allocator.get_statement(id) {
            Self::Expression(expression) => {
                let expression = Expression::perform_semantic_analysis(
                    *expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                low_allocator.allocate_statement(UnresolvedStatement::Expression(expression))
            }
            Self::Let(explicit_type, pattern, value) => {
                let explicit_type = explicit_type
                    .clone()
                    .map(|explicit_type| explicit_type.perform_semantic_analysis(ctx));

                let value_span = high_allocator.get_expression_span(*value);

                let Some(value) = Expression::perform_semantic_analysis(
                    *value,
                    high_allocator,
                    low_allocator,
                    ctx,
                ) else {
                    Pattern::destructure_unknown(*pattern, high_allocator, ctx);

                    return None;
                };

                let value_type = low_allocator.get_expression_type(value);

                if let Some(explicit_type) = &explicit_type {
                    value_type.assert_equals(ctx, value_span, explicit_type)?;
                }

                let variable_type = explicit_type.unwrap_or_else(|| value_type.clone());

                let pattern = Pattern::perform_semantic_analysis(
                    *pattern,
                    high_allocator,
                    low_allocator,
                    ctx,
                    &variable_type,
                )?;

                low_allocator.allocate_statement(UnresolvedStatement::Let(
                    variable_type,
                    pattern,
                    value,
                ))
            }
            Self::Append(target, value) => {
                let target = Expression::perform_semantic_analysis(
                    *target,
                    high_allocator,
                    low_allocator,
                    ctx,
                );
                let value = Expression::perform_semantic_analysis(
                    *value,
                    high_allocator,
                    low_allocator,
                    ctx,
                );

                let target = target?;
                let value = value?;

                low_allocator.allocate_statement(UnresolvedStatement::Append(target, value))
            }
            Self::Remove(target) => {
                let target = Expression::perform_semantic_analysis(
                    *target,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                low_allocator.allocate_statement(UnresolvedStatement::Remove(target))
            }
            Self::Break => {
                if ctx.loop_depth == 0 {
                    let span = high_allocator.get_statement_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Break),
                    );
                }

                low_allocator.allocate_statement(UnresolvedStatement::Break)
            }
            Self::Continue => {
                if ctx.loop_depth == 0 {
                    let span = high_allocator.get_statement_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Continue),
                    );
                }

                low_allocator.allocate_statement(UnresolvedStatement::Continue)
            }
            Self::Item(item) => {
                let item =
                    Item::perform_semantic_analysis(*item, high_allocator, low_allocator, ctx)?;

                low_allocator.allocate_statement(UnresolvedStatement::Item(item))
            }
        })
    }
}
