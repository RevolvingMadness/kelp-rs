use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{data_type::CSTDataType, expression::CSTExpression, pattern::CSTPattern},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTLetStatement, SyntaxKind::LetStatement);

impl<'a> CSTLetStatement<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::LetStatement);
        parser.bump_keyword("let");
        parser.skip_whitespace();

        if !CSTPattern::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.skip_whitespace();

        if parser.try_bump_char(':') {
            parser.skip_whitespace();

            if !CSTDataType::try_parse(parser) {
                parser.error("Expected data type");
            }

            parser.skip_whitespace();
        }

        let parsed_equals = parser.try_bump_char('=');
        if !parsed_equals {
            parser.error("Expected '='");
        }

        parser.skip_whitespace();

        if !CSTExpression::try_parse(parser) && parsed_equals {
            parser.recover_newline("Expected expression");
        }

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn let_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn pattern(&self) -> Option<CSTPattern<'a>> {
        self.children().find_map(CSTPattern::cast)
    }

    #[must_use]
    pub fn data_type(&self) -> Option<CSTDataType<'a>> {
        self.children().find_map(CSTDataType::cast)
    }

    #[must_use]
    pub fn value(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }
}
