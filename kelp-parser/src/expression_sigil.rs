use kelp_core::{parsed::supports_expression_sigil::ParsedSupportsExpressionSigil, span::Span};

use crate::{
    cst::CSTExpressionSigil,
    expression::{lower_expression, try_parse_expression},
    lower_context::{LowerContext, LowerError},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_expression_sigil(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('$') {
        return false;
    }

    parser.start_node(SyntaxKind::ExpressionSigil);

    parser.bump_char();

    parser.skip_whitespace();

    if !try_parse_expression(parser) {
        parser.error("Expected expression");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_sigil<T>(
    node: CSTExpressionSigil,
    ctx: &mut LowerContext,
) -> Option<ParsedSupportsExpressionSigil<T>> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(ParsedSupportsExpressionSigil::Sigil(expression))
}

pub fn assert_not_sigil<T>(
    sigil: ParsedSupportsExpressionSigil<T>,
    span: Span,
    ctx: &mut LowerContext,
) -> Option<T> {
    match sigil {
        ParsedSupportsExpressionSigil::Regular(value) => Some(value),
        ParsedSupportsExpressionSigil::Sigil(..) => {
            ctx.add_error(span, LowerError::ExpressionSigilNotAllowed)
        }
    }
}
