use kelp_core::expression::Expression;

use crate::{cst_node, lower::expression::CSTExpression, parser::Parser, syntax::SyntaxKind};

cst_node!(CSTListExpression, SyntaxKind::ListExpression);

impl<'a> CSTListExpression<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('[') {
            return false;
        }

        parser.start_node(SyntaxKind::ListExpression);
        parser.bump_char();

        let mut is_first = true;
        while !parser.is_eof() && parser.peek_char() != Some(']') {
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

            parser.skip_whitespace();
            is_first = false;
        }

        parser.expect_char(']', "Expected ']' to terminate array");
        parser.finish_node();
        true
    }

    pub fn expressions(&self) -> impl Iterator<Item = CSTExpression<'a>> {
        self.0.children().filter_map(CSTExpression::cast)
    }

    pub fn lower(self, text: &str) -> Vec<Expression> {
        self.expressions()
            .filter_map(|expression| expression.lower(text))
            .collect()
    }
}
