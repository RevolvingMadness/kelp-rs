use la_arena::Idx;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::{Expression, place::PlaceExpression},
    typed::arena::Typed,
};

pub type AssigneeExpressionId = Idx<Typed<AssigneeExpression>>;

#[derive(Debug, Clone)]
pub enum AssigneeExpression {
    Place(Box<PlaceExpression>),

    Tuple(Vec<Self>),
    Underscore,
}

impl AssigneeExpression {
    pub fn assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value_expression: Expression,
    ) {
        match self {
            Self::Place(expression) => expression.assign(datapack, ctx, value_expression),

            Self::Tuple(assignee_expressions) => {
                let Expression::Tuple(value_expressions) = value_expression else {
                    unreachable!();
                };

                for (assignee_expression, value_expression) in
                    assignee_expressions.into_iter().zip(value_expressions)
                {
                    assignee_expression.assign(datapack, ctx, value_expression);
                }
            }
            Self::Underscore => {}
        }
    }
}
