use crate::{
    high::{
        expression::{Expression, block::BlockExpression},
        pattern::Pattern,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::{
        data_type::DataType,
        expression::r#loop::{
            LoopExpression as MiddleLoopExpression, LoopExpressionKind as MiddleLoopExpressionKind,
        },
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub enum LoopExpressionKind {
    Predicate(Box<Expression>, Box<BlockExpression>),
    Infinite(Box<BlockExpression>),
    Iterator(bool, Pattern, Box<Expression>, Box<BlockExpression>),
}

impl LoopExpressionKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> LoopExpression {
        LoopExpression { span, kind: self }
    }
}

#[derive(Debug, Clone)]
pub struct LoopExpression {
    pub span: Span,
    pub kind: LoopExpressionKind,
}

impl LoopExpression {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, MiddleLoopExpression)> {
        let expression = match self.kind {
            LoopExpressionKind::Predicate(condition, body) => {
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

                let (_, _, body) = body?;

                MiddleLoopExpressionKind::Predicate(Box::new(condition), Box::new(body))
                    .with(DataType::Unit)
            }
            LoopExpressionKind::Infinite(body) => {
                ctx.loop_depth += 1;
                let body = body.perform_semantic_analysis(ctx);
                ctx.loop_depth -= 1;

                let (_, _, body) = body?;

                MiddleLoopExpressionKind::Infinite(Box::new(body)).with(DataType::Unit)
            }
            LoopExpressionKind::Iterator(reversed, pattern, iterable, body) => {
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

                ctx.loop_depth += 1;
                let Some((_, _, body)) = body.perform_semantic_analysis(ctx) else {
                    ctx.loop_depth -= 1;

                    ctx.exit_scope();

                    return None;
                };
                ctx.loop_depth -= 1;
                ctx.exit_scope();

                // TODO: Reorder semantic analysis

                MiddleLoopExpressionKind::Iterator(
                    reversed,
                    Box::new(pattern),
                    Box::new(iterable),
                    Box::new(body),
                )
                .with(DataType::Unit)
            }
        };

        Some((self.span, expression))
    }
}
