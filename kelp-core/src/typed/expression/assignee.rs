use la_arena::Idx;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::assignee::AssigneeExpression,
    parsed::semantic_analysis::SemanticAnalysisContext,
    span::Span,
    typed::{
        arena::{Typed, TypedAstArena},
        data_type::SemanticDataType,
        expression::place::{TypedPlaceExpression, TypedPlaceExpressionId},
    },
};

pub type TypedAssigneeExpressionId = Idx<Typed<TypedAssigneeExpression>>;

#[derive(Debug, Clone)]
pub enum TypedAssigneeExpression {
    Place(TypedPlaceExpressionId),

    Tuple(Vec<TypedAssigneeExpressionId>),
    Underscore,
}

impl TypedAssigneeExpression {
    #[must_use]
    pub fn resolve(
        id: TypedAssigneeExpressionId,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> AssigneeExpression {
        match arena.get_assignee_expression_value(id) {
            Self::Place(expression) => {
                let expression = TypedPlaceExpression::resolve(*expression, arena, datapack, ctx);

                AssigneeExpression::Place(Box::new(expression))
            }
            Self::Tuple(expressions) => {
                let expressions = expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, arena, datapack, ctx))
                    .collect();

                AssigneeExpression::Tuple(expressions)
            }
            Self::Underscore => AssigneeExpression::Underscore,
        }
    }

    #[must_use]
    pub fn perform_assignment_semantic_analysis(
        id: TypedAssigneeExpressionId,
        arena: &TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
    ) -> Option<()> {
        match arena.get_assignee_expression_value(id) {
            Self::Place(expression) => TypedPlaceExpression::perform_assignment_semantic_analysis(
                *expression,
                arena,
                ctx,
                value_span,
                value_type,
            ),
            Self::Tuple(..) => {
                let data_type = arena.get_assignee_expression_type(id);

                data_type.assert_equals(ctx, value_span, value_type)
            }
            Self::Underscore => Some(()),
        }
    }
}
