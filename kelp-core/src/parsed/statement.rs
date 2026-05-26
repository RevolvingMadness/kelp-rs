use la_arena::Idx;

use crate::ast_allocator::high::{HighAstAllocator, Spanned};
use crate::ast_allocator::low::LowAstAllocator;
use crate::parsed::data_type::DataType;
use crate::parsed::expression::{ParsedExpression, ParsedExpressionId};
use crate::parsed::item::Item;
use crate::parsed::pattern::Pattern;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::typed::statement::{LoopControlFlowKind, UnresolvedStatement};

pub type StatementId = Idx<Spanned<Statement>>;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ParsedExpressionId),
    Let(Option<DataType>, Idx<Pattern>, ParsedExpressionId),
    Append(ParsedExpressionId, ParsedExpressionId),
    Remove(ParsedExpressionId),
    Item(Idx<Item>),
    Break,
    Continue,
}

impl Statement {
    #[must_use]
    pub fn perform_semantic_analysis(
        id: StatementId,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<UnresolvedStatement>> {
        Some(match high_allocator.get_statement_value(id) {
            Self::Expression(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
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

                let Some(value) = ParsedExpression::perform_semantic_analysis(
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
                let target = ParsedExpression::perform_semantic_analysis(
                    *target,
                    high_allocator,
                    low_allocator,
                    ctx,
                );
                let value = ParsedExpression::perform_semantic_analysis(
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
                let target = ParsedExpression::perform_semantic_analysis(
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
