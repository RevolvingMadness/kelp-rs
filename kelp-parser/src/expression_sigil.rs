use kelp_core::{parsed::supports_expression_sigil::ParsedSupportsExpressionSigil, span::Span};

use crate::{
    cst::{CSTExpression, CSTExpressionSigil},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::{LowerContext, LowerError},
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTExpressionSigil {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('$') {
            return false;
        }

        parser.start_node(SyntaxKind::ExpressionSigil);

        parser.bump_char();

        parser.skip_whitespace();

        CSTExpression::expect(parser, "Expected expression");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTExpressionSigil {
    type Lowered = ParsedSupportsExpressionSigil<()>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expression = self.expression()?.lower(ctx)?;

        Some(ParsedSupportsExpressionSigil::Sigil(expression))
    }
}

pub fn assert_not_sigil<T>(
    sigil: ParsedSupportsExpressionSigil<T>,
    span: Span,
    ctx: &mut LowerContext,
) -> Option<T> {
    match sigil {
        ParsedSupportsExpressionSigil::Regular(value) => Some(value),
        ParsedSupportsExpressionSigil::Sigil(..) => {
            ctx.add_error(LowerError::ExpressionSigilNotAllowed { span })
        }
    }
}
