use la_arena::Idx;

use crate::{
    ast_allocator::low::{LowAstAllocator, Typed},
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::{
        expression::place::{ParsedPlaceExpression, ParsedPlaceExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    span::Span,
    typed::{data_type::SemanticDataType, expression::assignee::TypedAssigneeExpression},
};

pub type ParsedAssigneeExpressionId = Idx<Typed<ParsedAssigneeExpression>>;

#[derive(Debug, Clone)]
pub enum ParsedAssigneeExpression {
    Place(ParsedPlaceExpressionId),

    Tuple(Vec<ParsedAssigneeExpressionId>),
    Underscore,
}

impl ParsedAssigneeExpression {
    #[must_use]
    pub fn resolve(
        id: ParsedAssigneeExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> TypedAssigneeExpression {
        match allocator.get_assignee_expression_value(id) {
            Self::Place(expression) => {
                let expression =
                    ParsedPlaceExpression::resolve(*expression, allocator, datapack, ctx);

                TypedAssigneeExpression::Place(expression)
            }
            Self::Tuple(expressions) => {
                let expressions = expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, allocator, datapack, ctx))
                    .collect();

                TypedAssigneeExpression::Tuple(expressions)
            }
            Self::Underscore => TypedAssigneeExpression::Underscore,
        }
    }

    #[must_use]
    pub fn perform_assignment_semantic_analysis(
        id: ParsedAssigneeExpressionId,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
    ) -> Option<()> {
        match allocator.get_assignee_expression_value(id) {
            Self::Place(expression) => ParsedPlaceExpression::perform_assignment_semantic_analysis(
                *expression,
                allocator,
                ctx,
                value_span,
                value_type,
            ),
            Self::Tuple(..) => {
                let data_type = allocator.get_assignee_expression_type(id);

                data_type.assert_equals(ctx, value_span, value_type)
            }
            Self::Underscore => Some(()),
        }
    }
}
