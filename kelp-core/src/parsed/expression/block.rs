use crate::{
    parsed::{
        expression::ParsedExpression,
        semantic_analysis::SemanticAnalysisContext,
        statement::{ParsedStatement, ParsedStatementKind},
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    semantic::expression::unresolved::{SemanticExpression, SemanticExpressionKind},
};
use crate::semantic::data_type::SemanticDataType;

#[derive(Debug, Clone)]
pub struct ParsedBlockExpressionInfo {
    pub statements: Vec<ParsedStatement>,
    pub tail_expression: Option<Box<ParsedExpression>>,
}

impl ParsedBlockExpressionInfo {
    #[inline]
    #[must_use]
    pub const fn with_span(self, span: Span) -> ParsedBlockExpression {
        ParsedBlockExpression { span, info: self }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedBlockExpression {
    pub span: Span,
    pub info: ParsedBlockExpressionInfo,
}

impl ParsedBlockExpression {
    #[must_use]
    pub fn perform_semantic_analysis(
        mut self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, Option<Span>, SemanticExpression)> {
        ctx.enter_scope();

        for statement in &mut self.info.statements {
            let ParsedStatementKind::Item(item) = &mut statement.kind else {
                continue;
            };

            item.resolve_names(ctx);
        }

        for statement in &mut self.info.statements {
            let ParsedStatementKind::Item(item) = &mut statement.kind else {
                continue;
            };

            item.resolve_imports(ctx);
        }

        for statement in &mut self.info.statements {
            let ParsedStatementKind::Item(item) = &mut statement.kind else {
                continue;
            };

            item.resolve_types(ctx);
        }

        let body = self
            .info
            .statements
            .into_iter()
            .map(|statement| statement.perform_semantic_analysis(ctx))
            .collect_option_all::<Vec<_>>();

        let tail_expression = self
            .info
            .tail_expression
            .map(|tail_expression| tail_expression.perform_semantic_analysis(ctx));

        ctx.exit_scope();

        let body = body?;

        let tail_expression = match tail_expression {
            Some(tail_expression) => Some(tail_expression?),
            None => None,
        };

        let data_type = tail_expression
            .as_ref()
            .map_or(SemanticDataType::Unit, |(_, tail_expression)| {
                tail_expression.data_type.clone()
            });

        Some((
            self.span,
            tail_expression.as_ref().map(|(span, _)| *span),
            SemanticExpressionKind::Block(
                body,
                tail_expression.map(|(_, tail_expression)| Box::new(tail_expression)),
            )
            .with(data_type),
        ))
    }
}
