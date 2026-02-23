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
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::MCFNDeclarationStatement);
        parser.bump_keyword("mcfn".len());
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

    pub fn resource_location(&self) -> Option<CSTResourceLocation<'a>> {
        self.0.children().find_map(CSTResourceLocation::cast)
    }

    pub fn block_statement(&self) -> Option<CSTStatement<'a>> {
        self.0
            .children()
            .find(|n| n.is_kind(SyntaxKind::Block))
            .and_then(CSTStatement::cast)
    }
}
