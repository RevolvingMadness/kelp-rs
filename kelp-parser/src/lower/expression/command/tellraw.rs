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

        let parsed_inline_whitespace = parser.expect_inline_whitespace();

        if !parsed_inline_whitespace {
            parser.restore_state(state);

            return false;
        }

        let parsed_entity_selector = CSTEntitySelector::try_parse(parser);

        if !parsed_entity_selector {
            parser.restore_state(state);

            return false;
        }

        if parsed_entity_selector {
            parser.expect_inline_whitespace();
        } else {
            parser.skip_inline_whitespace();
        }

        if !CSTExpression::try_parse(parser) {
            parser.recover_newline("Expected expression");
        }

        parser.finish_node();

        true
    }

    pub fn tellraw_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }

    pub fn message(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }
}
