use la_arena::Idx;

use crate::{
    ast_allocator::low::{LowAstAllocator, Typed},
    compile_context::CompileContext,
    datapack::Datapack,
    high::{
        expression::place::{UnresolvedPlaceExpression, UnresolvedPlaceExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::assignee::ResolvedAssigneeExpression,
    },
    span::Span,
};

pub type UnresolvedAssigneeExpressionId = Idx<Typed<UnresolvedAssigneeExpression>>;

#[derive(Debug, Clone)]
pub enum UnresolvedAssigneeExpression {
    Place(UnresolvedPlaceExpressionId),

    Tuple(Vec<UnresolvedAssigneeExpressionId>),
    Underscore,
}

impl UnresolvedAssigneeExpression {
    #[must_use]
    pub fn resolve(
        id: UnresolvedAssigneeExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedAssigneeExpression {
        match allocator.get_assignee_expression_value(id) {
            Self::Place(expression) => {
                let expression =
                    UnresolvedPlaceExpression::resolve(*expression, allocator, datapack, ctx);

                ResolvedAssigneeExpression::Place(expression)
            }
            Self::Tuple(expressions) => {
                let expressions = expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, allocator, datapack, ctx))
                    .collect();

                ResolvedAssigneeExpression::Tuple(expressions)
            }
            Self::Underscore => ResolvedAssigneeExpression::Underscore,
        }
    }

    #[must_use]
    pub fn perform_assignment_semantic_analysis(
        id: UnresolvedAssigneeExpressionId,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &UnresolvedDataType,
    ) -> Option<()> {
        match allocator.get_assignee_expression_value(id) {
            Self::Place(expression) => {
                UnresolvedPlaceExpression::perform_assignment_semantic_analysis(
                    *expression,
                    allocator,
                    ctx,
                    value_span,
                    value_type,
                )
            }
            Self::Tuple(..) => {
                let data_type = allocator.get_assignee_expression_type(id);

                data_type.assert_equals(ctx, value_span, value_type)
            }
            Self::Underscore => Some(()),
        }
    }
}
