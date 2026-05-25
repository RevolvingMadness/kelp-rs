use kelp_core::high::expression::Expression;
use la_arena::Idx;

use crate::{
    cst::CSTReturnExpression,
    expression::{lower_expression, try_parse_expression},
    lower_context::LowerContext,
    parser::Parser,
    span::{span_of_cst_node, text_range_to_span},
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
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let keyword_span = text_range_to_span(node.return_keyword_token()?.text_range());
    let full_span = span_of_cst_node(&node);

    let (expression_span, expression) = match node.expression() {
        Some(expression) => {
            let span = span_of_cst_node(&expression);
            let expr = lower_expression(expression, ctx)?;

            (span, Some(expr))
        }
        None => (full_span, None),
    };

    Some(ctx.allocator.allocate_expression(
        full_span,
        Expression::Return(keyword_span, expression_span, expression),
    ))
}
