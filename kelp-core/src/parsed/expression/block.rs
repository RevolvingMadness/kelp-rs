use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        expression::{ParsedExpression, ParsedExpressionId},
        item::ParsedItem,
        semantic_analysis::SemanticAnalysisContext,
        statement::{Statement, StatementId},
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
    typed::arena::TypedAstArena,
    typed::{
        data_type::SemanticDataType,
        expression::{TypedExpression, TypedExpressionId},
    },
};

#[derive(Debug, Clone)]
pub struct BlockExpressionInfo {
    pub statements: Vec<StatementId>,
    pub tail_expression: Option<ParsedExpressionId>,
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<(Span, Option<Span>, TypedExpressionId)> {
        ctx.enter_scope();

        let items = self
            .info
            .statements
            .iter()
            .copied()
            .filter_map(|statement| {
                let statement = parsed_arena.get_statement_value(statement);

                if let Statement::Item(item) = statement {
                    Some(*item)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for item in items.iter().copied() {
            ParsedItem::resolve_names(item, parsed_arena, ctx);
        }

        for item in items.iter().copied() {
            ParsedItem::resolve_imports(item, parsed_arena, ctx);
        }

        for item in items.iter().copied() {
            ParsedItem::resolve_types(item, parsed_arena, ctx);
        }

        for item in items.iter().copied() {
            ParsedItem::resolve_value_types(item, parsed_arena, ctx);
        }

        let body = self
            .info
            .statements
            .iter()
            .copied()
            .map(|statement| {
                Statement::perform_semantic_analysis(statement, parsed_arena, typed_arena, ctx)
            })
            .collect_option_all::<Vec<_>>();

        let tail_expression = self.info.tail_expression.map(|tail_expression| {
            ParsedExpression::perform_semantic_analysis(
                tail_expression,
                parsed_arena,
                typed_arena,
                ctx,
            )
        });

        ctx.exit_scope();

        let body = body?;

        let tail_expression = match tail_expression {
            Some(tail_expression) => Some(tail_expression?),
            None => None,
        };

        let data_type = tail_expression.map_or(SemanticDataType::Unit, |tail_expression| {
            typed_arena.get_expression_type(tail_expression).clone()
        });

        Some((
            self.span,
            self.info
                .tail_expression
                .map(|tail_expression| parsed_arena.get_expression_span(tail_expression)),
            typed_arena
                .allocate_expression(TypedExpression::Block(body, tail_expression), data_type),
        ))
    }
}
