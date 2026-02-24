use kelp_core::span::Span;

use crate::{
    cst_node, lower::entity_selector::CSTEntitySelector, parser::Parser, syntax::SyntaxKind,
};

cst_node!(CSTScoreExpression, SyntaxKind::ScoreExpression);

impl<'a> CSTScoreExpression<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::ScoreExpression);
        parser.bump_identifier("score");

        if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.expect_inline_whitespace();

        parser.expect_identifier("Expected scoreboard objective");

        parser.finish_node();

        true
    }

    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }

    pub fn message_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn message<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }
}
