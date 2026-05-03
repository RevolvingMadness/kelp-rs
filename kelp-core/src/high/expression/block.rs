use crate::{
    high::{
        expression::Expression, semantic_analysis::SemanticAnalysisContext, statement::Statement,
    },
    low::{
        data_type::DataType,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionKind},
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub struct BlockExpressionInfo {
    pub statements: Vec<Statement>,
    pub tail_expression: Option<Box<Expression>>,
}

impl BlockExpressionInfo {
    #[inline]
    #[must_use]
    pub const fn with_span(self, span: Span) -> BlockExpression {
        BlockExpression { span, info: self }
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub span: Span,
    pub info: BlockExpressionInfo,
}

impl BlockExpression {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, Option<Span>, UnresolvedExpression)> {
        ctx.enter_scope();

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
            .map_or(DataType::Unit, |(_, tail_expression)| {
                tail_expression.data_type.clone()
            });

        Some((
            self.span,
            tail_expression.as_ref().map(|(span, _)| *span),
            UnresolvedExpressionKind::Block(
                body,
                tail_expression.map(|(_, tail_expression)| Box::new(tail_expression)),
            )
            .with(data_type),
        ))
    }
}
