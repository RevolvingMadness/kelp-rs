use la_arena::Idx;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    high::{
        expression::place::UnresolvedPlaceExpression, semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::assignee::ResolvedAssigneeExpression,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub enum UnresolvedAssigneeExpression {
    Place(Idx<UnresolvedPlaceExpression>),

    Tuple(Vec<Idx<Self>>),
    Underscore,
}

impl UnresolvedAssigneeExpression {
    #[must_use]
    pub fn resolve(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedAssigneeExpression {
        match allocator.get_assignee_expression(id) {
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
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &UnresolvedDataType,
    ) -> Option<()> {
        match allocator.get_assignee_expression(id) {
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
