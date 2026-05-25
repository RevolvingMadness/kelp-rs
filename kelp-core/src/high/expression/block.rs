use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        expression::{Expression, ExpressionId},
        item::Item,
        semantic_analysis::SemanticAnalysisContext,
        statement::{Statement, StatementId},
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
};

#[derive(Debug, Clone)]
pub struct BlockExpressionInfo {
    pub statements: Vec<StatementId>,
    pub tail_expression: Option<ExpressionId>,
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
        &self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, Option<Span>, Idx<UnresolvedExpression>)> {
        ctx.enter_scope();

        let items = self
            .info
            .statements
            .iter()
            .copied()
            .filter_map(|statement| {
                let statement = high_allocator.get_statement_value(statement);

                if let Statement::Item(item) = statement {
                    Some(*item)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for item in items.iter().copied() {
            Item::resolve_names(item, high_allocator, ctx);
        }

        for item in items.iter().copied() {
            Item::resolve_imports(item, high_allocator, ctx);
        }

        for item in items.iter().copied() {
            Item::resolve_types(item, high_allocator, ctx);
        }

        for item in items.iter().copied() {
            Item::resolve_value_types(item, high_allocator, ctx);
        }

        let body = self
            .info
            .statements
            .iter()
            .copied()
            .map(|statement| {
                Statement::perform_semantic_analysis(statement, high_allocator, low_allocator, ctx)
            })
            .collect_option_all::<Vec<_>>();

        let tail_expression = self.info.tail_expression.map(|tail_expression| {
            Expression::perform_semantic_analysis(
                tail_expression,
                high_allocator,
                low_allocator,
                ctx,
            )
        });

        ctx.exit_scope();

        let body = body?;

        let tail_expression = match tail_expression {
            Some(tail_expression) => Some(tail_expression?),
            None => None,
        };

        let data_type = tail_expression.map_or(UnresolvedDataType::Unit, |tail_expression| {
            low_allocator.get_expression_type(tail_expression).clone()
        });

        Some((
            self.span,
            self.info
                .tail_expression
                .map(|tail_expression| high_allocator.get_expression_span(tail_expression)),
            low_allocator.allocate_expression(
                UnresolvedExpression::Block(body, tail_expression),
                data_type,
            ),
        ))
    }
}
