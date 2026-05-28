use crate::semantic::data_type::SemanticDataType;
use crate::{
    parsed::{
        expression::ParsedExpression,
        semantic_analysis::SemanticAnalysisContext,
        statement::{ParsedStatement, ParsedStatementKind},
    },
    semantic::expression::{SemanticExpression, SemanticExpressionKind},
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

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
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, Option<Span>, SemanticExpression)> {
        ctx.enter_scope();

        let mut items = Vec::new();
        let mut statements = Vec::new();

        for statement in self.info.statements {
            match statement.kind {
                ParsedStatementKind::Item(item) => items.push(*item),
                _ => statements.push(statement),
            }
        }

        let Some(mut items) = items
            .into_iter()
            .map(|item| item.resolve_names(ctx))
            .collect_option_all::<Vec<_>>()
        else {
            ctx.exit_scope();

            return None;
        };

        for item in &mut items {
            item.resolve_imports(ctx);
        }

        let items = items
            .into_iter()
            .map(|item| item.resolve_types(ctx))
            .collect::<Vec<_>>();

        let mut failed = false;

        for item in items {
            if item.perform_semantic_analysis(ctx).is_none() {
                failed = true;
            }
        }

        if failed {
            return None;
        }

        let body = statements
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
