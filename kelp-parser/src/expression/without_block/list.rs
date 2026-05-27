use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTExpression, CSTListExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTListExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('[') {
            return false;
        }

        parser.start_node(SyntaxKind::ListExpression);
        parser.bump_char();

        let mut is_first = true;

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some(']') {
                break;
            }

            if !is_first {
                if parser.try_bump_char(',') {
                    parser.skip_whitespace();
                } else {
                    parser.error("Expected ',' between array elements");
                }
            }

            if parser.peek_char() == Some(']') {
                break;
            }

            if !CSTExpression::try_parse(parser) {
                parser.error("Expected expression");
                parser.bump_until_char(&[',', ']']);
            }

            is_first = false;
        }

        parser.expect_char(']', "Expected ']' to terminate array");
        parser.finish_node();
        true
    }
}

impl LowerableAstNode for CSTListExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expressions = self
            .expressions()
            .filter_map(|expression| expression.lower(ctx))
            .collect();

        Some(ParsedExpressionKind::List(expressions).with_span(self.span()))
    }
}
