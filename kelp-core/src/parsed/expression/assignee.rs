use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::{
        expression::place::SemanticPlaceExpression, semantic_analysis::SemanticAnalysisContext,
    },
    span::Span,
    semantic::expression::assignee::ResolvedAssigneeExpression,
};
use crate::semantic::data_type::SemanticDataType;

#[derive(Debug, Clone)]
pub enum UnresolvedAssigneeExpressionKind {
    Place(SemanticPlaceExpression),

    Tuple(Vec<SemanticAssigneeExpression>),
    Underscore,
}

impl UnresolvedAssigneeExpressionKind {
    #[inline]
    #[must_use]
    pub const fn with(self, data_type: SemanticDataType) -> SemanticAssigneeExpression {
        SemanticAssigneeExpression {
            kind: self,
            data_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticAssigneeExpression {
    pub kind: UnresolvedAssigneeExpressionKind,
    pub data_type: SemanticDataType,
}

impl SemanticAssigneeExpression {
    #[must_use]
    pub fn resolve(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedAssigneeExpression {
        match self.kind {
            UnresolvedAssigneeExpressionKind::Place(expression) => {
                let expression = expression.resolve(datapack, ctx);

                ResolvedAssigneeExpression::Place(expression)
            }
            UnresolvedAssigneeExpressionKind::Tuple(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|expression| expression.resolve(datapack, ctx))
                    .collect();

                ResolvedAssigneeExpression::Tuple(expressions)
            }
            UnresolvedAssigneeExpressionKind::Underscore => ResolvedAssigneeExpression::Underscore,
        }
    }

    #[must_use]
    pub fn perform_assignment_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        value_type: &SemanticDataType,
    ) -> Option<()> {
        match &self.kind {
            UnresolvedAssigneeExpressionKind::Place(expression) => {
                expression.perform_assignment_semantic_analysis(ctx, value_span, value_type)
            }
            UnresolvedAssigneeExpressionKind::Tuple(..) => {
                self.data_type.assert_equals(ctx, value_span, value_type)
            }
            UnresolvedAssigneeExpressionKind::Underscore => Some(()),
        }
    }
}
