use kelp_core::{
    high::{
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        supports_expression_sigil::SupportsExpressionSigil,
    },
    span::Span,
};

use crate::{
    cst::CSTExpressionSigil,
    expression::{lower_expression, try_parse_expression},
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
    ctx: &mut SemanticAnalysisContext,
) -> Option<SupportsExpressionSigil<T>> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(SupportsExpressionSigil::Sigil(expression))
}

pub fn assert_not_sigil<T>(
    sigil: SupportsExpressionSigil<T>,
    span: Span,
    ctx: &mut SemanticAnalysisContext,
) -> Option<T> {
    match sigil {
        SupportsExpressionSigil::Regular(value) => Some(value),
        SupportsExpressionSigil::Sigil(_) => {
            ctx.add_error(span, SemanticAnalysisError::ExpressionSigilNotAllowed)
        }
    }
}
