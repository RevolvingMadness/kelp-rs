use la_arena::Idx;

use crate::parsed::arena::{ParsedAstArena, Spanned};
use crate::parsed::data_type::ParsedDataType;
use crate::parsed::expression::{ParsedExpression, ParsedExpressionId};
use crate::parsed::item::ParsedItem;
use crate::parsed::pattern::Pattern;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::typed::arena::TypedAstArena;
use crate::typed::statement::{LoopControlFlowKind, TypedStatement};

pub type StatementId = Idx<Spanned<Statement>>;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ParsedExpressionId),
    Let(Option<ParsedDataType>, Idx<Pattern>, ParsedExpressionId),
    Append(ParsedExpressionId, ParsedExpressionId),
    Remove(ParsedExpressionId),
    Item(Idx<ParsedItem>),
    Break,
    Continue,
}

impl Statement {
    #[must_use]
    pub fn perform_semantic_analysis(
        id: StatementId,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<TypedStatement>> {
        Some(match parsed_arena.get_statement_value(id) {
            Self::Expression(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    *expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                typed_arena.allocate_statement(TypedStatement::Expression(expression))
            }
            Self::Let(explicit_type, pattern, value) => {
                let explicit_type = explicit_type
                    .clone()
                    .map(|explicit_type| explicit_type.perform_semantic_analysis(ctx));

                let value_span = parsed_arena.get_expression_span(*value);

                let Some(value) = ParsedExpression::perform_semantic_analysis(
                    *value,
                    parsed_arena,
                    typed_arena,
                    ctx,
                ) else {
                    Pattern::destructure_unknown(*pattern, parsed_arena, ctx);

                    return None;
                };

                let value_type = typed_arena.get_expression_type(value);

                if let Some(explicit_type) = &explicit_type {
                    value_type.assert_equals(ctx, value_span, explicit_type)?;
                }

                let variable_type = explicit_type.unwrap_or_else(|| value_type.clone());

                let pattern = Pattern::perform_semantic_analysis(
                    *pattern,
                    parsed_arena,
                    typed_arena,
                    ctx,
                    &variable_type,
                )?;

                typed_arena.allocate_statement(TypedStatement::Let(variable_type, pattern, value))
            }
            Self::Append(target, value) => {
                let target = ParsedExpression::perform_semantic_analysis(
                    *target,
                    parsed_arena,
                    typed_arena,
                    ctx,
                );
                let value = ParsedExpression::perform_semantic_analysis(
                    *value,
                    parsed_arena,
                    typed_arena,
                    ctx,
                );

                let target = target?;
                let value = value?;

                typed_arena.allocate_statement(TypedStatement::Append(target, value))
            }
            Self::Remove(target) => {
                let target = ParsedExpression::perform_semantic_analysis(
                    *target,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                typed_arena.allocate_statement(TypedStatement::Remove(target))
            }
            Self::Break => {
                if ctx.loop_depth == 0 {
                    let span = parsed_arena.get_statement_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Break),
                    );
                }

                typed_arena.allocate_statement(TypedStatement::Break)
            }
            Self::Continue => {
                if ctx.loop_depth == 0 {
                    let span = parsed_arena.get_statement_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Continue),
                    );
                }

                typed_arena.allocate_statement(TypedStatement::Continue)
            }
            Self::Item(item) => {
                let item =
                    ParsedItem::perform_semantic_analysis(*item, parsed_arena, typed_arena, ctx)?;

                typed_arena.allocate_statement(TypedStatement::Item(item))
            }
        })
    }
}
