use crate::{
    cst_node,
    lower::{expression::CSTExpression, statement::CSTStatement},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTWhileStatement, SyntaxKind::WhileStatement);

impl<'a> CSTWhileStatement<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::WhileStatement);
        parser.bump_keyword("while".len());
        parser.skip_inline_whitespace();

        if !CSTExpression::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.skip_inline_whitespace();

        if !CSTStatement::try_parse(parser) {
            parser.recover_newline("Expected statement");
        }

        parser.finish_node();

        true
    }

    pub fn condition(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn body(&self) -> Option<CSTStatement<'a>> {
        self.0.children().rev().find_map(CSTStatement::cast)
    }
}
