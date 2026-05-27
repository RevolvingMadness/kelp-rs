use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTReturnExpression,
    expression::{lower_expression, try_parse_expression},
    extension_traits::{AstNodeExt, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_return_expression(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::ReturnExpression);
    parser.bump_str(SyntaxKind::ReturnKeyword, "return");

    let state = parser.save_state();

    if parser.try_skip_whitespace() && !try_parse_expression(parser) {
        state.restore(parser);
    }

    parser.finish_node();

    true
}
#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_return_expression(
    node: CSTReturnExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let keyword_span = node.return_token()?.span();
    let full_span = node.span();

    let (expression_span, expression) = match node.expression() {
        Some(expression) => {
            let span = expression.span();
            let expr = lower_expression(expression, ctx)?;

            (span, Some(expr))
        }
        None => (full_span, None),
    };

    Some(
        ParsedExpressionKind::Return(keyword_span, expression_span, expression.map(Box::new))
            .with_span(full_span),
    )
}
