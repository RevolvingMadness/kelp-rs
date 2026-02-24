use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{
        expression::CSTExpression,
        statement::{CSTStatement, block::CSTBlockStatement},
    },
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTIfStatement, SyntaxKind::IfStatement);

impl<'a> CSTIfStatement<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::IfStatement);
        parser.bump_keyword("if");
        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.skip_whitespace();

        if !CSTBlockStatement::try_parse(parser) {
            parser.recover_newline("Expected block statement");
        }

        parser.skip_whitespace();

        if let Some("else") = parser.peek_identifier() {
            parser.bump_keyword("else");

            parser.skip_whitespace();

            if parser.peek_char() == Some('{') {
                if !CSTBlockStatement::try_parse(parser) {
                    parser.recover_newline("Expected block statement");
                }
            } else if parser.peek_identifier() == Some("if") {
                if !CSTIfStatement::try_parse(parser) {
                    parser.recover_newline("Expected if statement");
                }
            } else {
                parser.recover_newline("Expected block or if statement");
            }
        }

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn if_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn else_keyword_span(&self) -> Option<Span> {
        self.0
            .children_tokens()
            .filter_map(|token| {
                if token.kind == SyntaxKind::Keyword {
                    Some(token.span)
                } else {
                    None
                }
            })
            .nth(1)
    }

    #[must_use]
    pub fn condition(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    #[must_use]
    pub fn body(&self) -> Option<CSTStatement<'a>> {
        self.children().filter_map(CSTStatement::cast).nth(1)
    }

    #[must_use]
    pub fn else_body(&self) -> Option<CSTStatement<'a>> {
        self.children().filter_map(CSTStatement::cast).nth(2)
    }
}
