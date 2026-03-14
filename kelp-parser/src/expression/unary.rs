use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    operator::UnaryOperator,
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTUnaryExpression, expression::lower_expression, span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unary_expression(
    node: CSTUnaryExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let operator = match node.operator()?.kind() {
        SyntaxKind::ExclamationMark => UnaryOperator::Invert,
        SyntaxKind::Minus => UnaryOperator::Negate,
        SyntaxKind::Star => UnaryOperator::Dereference,
        SyntaxKind::Ampersand => UnaryOperator::Reference,
        _ => return None,
    };

    let operand = lower_expression(node.expression()?, ctx)?;

    Some(ExpressionKind::Unary(operator, Box::new(operand)).with_span(span))
}
