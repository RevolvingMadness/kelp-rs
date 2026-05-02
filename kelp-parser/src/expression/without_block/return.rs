use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTReturnExpression,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_return_expression(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::ReturnExpression);
    parser.bump_str(SyntaxKind::ReturnKeyword, "return");

    let state = parser.save_state();

    if parser.try_skip_whitespace() && !try_parse_expression(parser) {
        parser.restore_state(state);
    }

    parser.finish_node();

    true
}
#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_return_expression(
    node: CSTReturnExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let full_span = span_of_cst_node(&node);

    let (span, expression) = match node.expression() {
        Some(expression) => {
            let span = span_of_cst_node(&expression);
            let expr = lower_expression(expression, ctx)?;

            (span, Some(expr))
        }
        None => (full_span, None),
    };

    Some(ExpressionKind::Return(span, expression.map(Box::new)).with_span(full_span))
}
