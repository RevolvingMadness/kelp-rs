use crate::parsed::data_type::ParsedDataType;
use crate::parsed::expression::ParsedExpression;
use crate::parsed::item::ParsedItem;
use crate::parsed::pattern::ParsedPattern;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::semantic::statement::{LoopControlFlowKind, UnresolvedStatement};
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum ParsedStatementKind {
    Expression(ParsedExpression),
    Let(Option<ParsedDataType>, ParsedPattern, ParsedExpression),
    Append(ParsedExpression, Box<ParsedExpression>),
    Remove(ParsedExpression),
    Item(Box<ParsedItem>),
    Break,
    Continue,
}

impl ParsedStatementKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> ParsedStatement {
        ParsedStatement { span, kind: self }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStatement {
    pub span: Span,
    pub kind: ParsedStatementKind,
}

impl ParsedStatement {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<UnresolvedStatement> {
        Some(match self.kind {
            ParsedStatementKind::Expression(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Expression(expression)
            }
            ParsedStatementKind::Let(explicit_type, pattern, value) => {
                let explicit_type =
                    explicit_type.map(|explicit_type| explicit_type.perform_semantic_analysis(ctx));

                let Some((value_span, value)) = value.perform_semantic_analysis(ctx) else {
                    pattern.kind.destructure_unknown(ctx);

                    return None;
                };

                if let Some(explicit_type) = &explicit_type {
                    value
                        .data_type
                        .assert_equals(ctx, value_span, explicit_type)?;
                }

                let variable_type = explicit_type.unwrap_or_else(|| value.data_type.clone());

                let pattern = pattern.perform_semantic_analysis(ctx, &variable_type)?;

                UnresolvedStatement::Let(variable_type, pattern, Box::new(value))
            }
            ParsedStatementKind::Append(target, value) => {
                let target = target.perform_semantic_analysis(ctx);
                let value = value.perform_semantic_analysis(ctx);

                let (_, target) = target?;
                let (_, value) = value?;

                UnresolvedStatement::Append(target, Box::new(value))
            }
            ParsedStatementKind::Remove(target) => {
                let (_, target) = target.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Remove(target)
            }
            ParsedStatementKind::Break => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Break),
                    );
                }

                UnresolvedStatement::Break
            }
            ParsedStatementKind::Continue => {
                if ctx.loop_depth == 0 {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::ControlFlowNotInLoop(LoopControlFlowKind::Continue),
                    );
                }

                UnresolvedStatement::Continue
            }
            ParsedStatementKind::Item(item) => {
                let item = item.perform_semantic_analysis(ctx)?;

                UnresolvedStatement::Item(Box::new(item))
            }
        })
    }

    #[must_use]
    pub const fn new(span: Span, kind: ParsedStatementKind) -> Self {
        Self { span, kind }
    }
}
