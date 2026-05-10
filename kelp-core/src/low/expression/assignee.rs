use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::{place::ResolvedPlaceExpression, resolved::ResolvedExpression},
};

#[derive(Debug, Clone)]
pub enum ResolvedAssigneeExpression {
    Place(ResolvedPlaceExpression),

    Tuple(Vec<Self>),
    Underscore,
}

impl ResolvedAssigneeExpression {
    pub fn assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        value_expression: ResolvedExpression,
    ) {
        match self {
            Self::Place(expression) => expression.assign(datapack, ctx, value_expression),

            Self::Tuple(assignee_expressions) => {
                let ResolvedExpression::Tuple(value_expressions) = value_expression else {
                    unreachable!();
                };

                for (assignee_expression, value_expression) in assignee_expressions
                    .into_iter()
                    .zip(value_expressions.into_iter())
                {
                    assignee_expression.assign(datapack, ctx, value_expression);
                }
            }
            Self::Underscore => {}
        }
    }
}
