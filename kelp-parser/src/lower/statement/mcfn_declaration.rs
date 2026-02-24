use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{
        resource_location::CSTResourceLocation,
        statement::{CSTStatement, block::CSTBlockStatement},
    },
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(
    CSTMCFNDeclarationStatement,
    SyntaxKind::MCFNDeclarationStatement
);

impl<'a> CSTMCFNDeclarationStatement<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::MCFNDeclarationStatement);
        parser.bump_keyword("mcfn");
        parser.expect_inline_whitespace();

        if !CSTResourceLocation::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.skip_inline_whitespace();
        CSTBlockStatement::try_parse(parser);
        parser.finish_node();

        true
    }

    #[must_use]
    pub fn mcfn_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn resource_location(&self) -> Option<CSTResourceLocation<'a>> {
        self.children().find_map(CSTResourceLocation::cast)
    }

    #[must_use]
    pub fn block_statement(&self) -> Option<CSTStatement<'a>> {
        self.0
            .children()
            .find(|node| node.is_kind(SyntaxKind::BlockStatement))
            .and_then(CSTStatement::cast)
    }
}
