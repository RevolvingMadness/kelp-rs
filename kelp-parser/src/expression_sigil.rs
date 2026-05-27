use kelp_core::{parsed::supports_expression_sigil::ParsedSupportsExpressionSigil, span::Span};

use crate::{
    cst::{CSTExpression, CSTExpressionSigil},
    extension_traits::{LowerableAstNode, ParsableAstNode},
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

    if !CSTExpression::try_parse(parser) {
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
    let expression = node.expression()?.lower(ctx)?;

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
