use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{entity_selector::CSTEntitySelector, expression::CSTExpression},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(
    CSTTellrawCommandExpression,
    SyntaxKind::TellrawCommandExpression
);

impl<'a> CSTTellrawCommandExpression<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::TellrawCommandExpression);
        parser.bump_keyword("tellraw");

        if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        let parsed_whitespace = parser.expect_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            if parsed_whitespace {
                parser.recover_newline("Expected expression");
            } else {
                parser.bump_until_newline();
            }
        }

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn tellraw_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.children().find_map(CSTEntitySelector::cast)
    }

    #[must_use]
    pub fn message(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }
}
