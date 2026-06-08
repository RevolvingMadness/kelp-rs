use crate::semantic::data_type::SemanticDataType;
use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::{
        expression::place::ParsedPlaceExpression, semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::assignee::SemanticAssigneeExpression,
    span::Span,
};

#[derive(Debug, Clone)]
pub enum ParsedAssigneeExpressionKind {
    Place(ParsedPlaceExpression),

    Tuple(Vec<ParsedAssigneeExpression>),
    Underscore,
}

impl ParsedAssigneeExpressionKind {
    #[inline]
    #[must_use]
    pub const fn with(self, data_type: SemanticDataType) -> ParsedAssigneeExpression {
        ParsedAssigneeExpression {
            kind: self,
            data_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedAssigneeExpression {
    pub kind: ParsedAssigneeExpressionKind,
    pub data_type: SemanticDataType,
}

impl ParsedAssigneeExpression {
    #[must_use]
    pub fn resolve(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> SemanticAssigneeExpression {
        match self.kind {
            ParsedAssigneeExpressionKind::Place(expression) => {
                let expression = expression.resolve(datapack, ctx);

                SemanticAssigneeExpression::Place(expression)
            }
            ParsedAssigneeExpressionKind::Tuple(expressions) => {
                let expressions = expressions
                    .into_iter()
                    .map(|expression| expression.resolve(datapack, ctx))
                    .collect();

                SemanticAssigneeExpression::Tuple(expressions)
            }
            ParsedAssigneeExpressionKind::Underscore => SemanticAssigneeExpression::Underscore,
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
            ParsedAssigneeExpressionKind::Place(expression) => {
                expression.perform_assignment_semantic_analysis(ctx, value_span, value_type)
            }
            ParsedAssigneeExpressionKind::Tuple(..) => {
                self.data_type.assert_equals(ctx, value_span, value_type)
            }
            ParsedAssigneeExpressionKind::Underscore => Some(()),
        }
    }
}
