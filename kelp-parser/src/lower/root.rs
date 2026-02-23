use kelp_core::statement::Statement;

use crate::{cst_node, lower::statement::CSTStatement, parser::Parser, syntax::SyntaxKind};

cst_node!(CSTRoot, SyntaxKind::Root);

impl<'a> CSTRoot<'a> {
    pub(crate) fn parse(parser: &mut Parser) {
        parser.start_node(SyntaxKind::Root);

        parser.skip_whitespace();

        let mut is_first = true;

        while !parser.is_eof() {
            if !is_first {
                let _ =
                    parser.expect_newline_whitespace("Expected newline to mark end of statement");
            }

            let _ = CSTStatement::try_parse(parser);

            is_first = false;
        }

        parser.finish_node();
    }

    pub fn lower(self) -> Vec<Statement> {
        self.statements()
            .into_iter()
            .filter_map(CSTStatement::lower)
            .collect()
    }

    pub fn statements(&self) -> Vec<CSTStatement<'a>> {
        self.0.children().filter_map(CSTStatement::cast).collect()
    }
}
