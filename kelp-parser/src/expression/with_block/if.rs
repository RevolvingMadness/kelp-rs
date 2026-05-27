use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTBlockExpression, CSTExpression, CSTIfExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTIfExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let Some("if") = parser.peek_identifier() else {
            return false;
        };

        let state = parser.save_state();

        parser.start_node(SyntaxKind::IfExpression);
        parser.bump_str(SyntaxKind::IfKeyword, "if");
        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            state.restore(parser);

            return false;
        }

        parser.skip_inline_whitespace();

        if !CSTBlockExpression::try_parse(parser) {
            parser.recover_not_whitespace("Expected block statement");
        }

        parser.skip_inline_whitespace();

        if parser.peek_identifier() == Some("else") {
            parser.bump_str(SyntaxKind::ElseKeyword, "else");

            parser.skip_inline_whitespace();

            if parser.peek_char() == Some('{') {
                if !CSTBlockExpression::try_parse(parser) {
                    parser.recover_not_whitespace("Expected block statement");
                }
            } else if parser.peek_identifier() == Some("if") {
                if !Self::try_parse(parser) {
                    parser.recover_not_whitespace("Expected if statement");
                }
            } else {
                parser.recover_not_whitespace("Expected block or if statement");
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTIfExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let condition = self.condition()?.lower(ctx)?;
        let body = self.body()?.lower(ctx)?;
        let else_body = self
            .else_body_block()
            .and_then(|expression| {
                let span = expression.span();

                let expression = expression.lower(ctx)?;

                Some(ParsedExpressionKind::Block(expression).with_span(span))
            })
            .or_else(|| self.else_body_if()?.lower(ctx))
            .map(Box::new);

        Some(
            ParsedExpressionKind::If {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body,
            }
            .with_span(self.span()),
        )
    }
}
