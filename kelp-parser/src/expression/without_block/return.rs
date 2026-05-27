use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTExpression, CSTReturnExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTReturnExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::ReturnExpression);
        parser.bump_str(SyntaxKind::ReturnKeyword, "return");

        let state = parser.save_state();

        if parser.try_skip_whitespace() && !CSTExpression::try_parse(parser) {
            state.restore(parser);
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTReturnExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let keyword_span = self.return_token()?.span();

        let (expression_span, expression) = match self.expression() {
            Some(expression) => {
                let span = expression.span();
                let expr = expression.lower(ctx)?;

                (span, Some(expr))
            }
            None => (self.span(), None),
        };

        Some(
            ParsedExpressionKind::Return(keyword_span, expression_span, expression.map(Box::new))
                .with_span(self.span()),
        )
    }
}
