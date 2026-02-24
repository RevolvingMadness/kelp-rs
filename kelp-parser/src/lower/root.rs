use kelp_core::statement::Statement;

use crate::{
    cst_node, lower::statement::CSTStatement, parser::Parser, semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

cst_node!(CSTRoot, SyntaxKind::Root);

impl<'a> CSTRoot<'a> {
    pub fn parse(parser: &mut Parser) {
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

    #[must_use]
    pub fn lower(self, text: &str) -> Vec<Statement> {
        self.statements()
            .into_iter()
            .filter_map(|statement| statement.lower(text))
            .collect()
    }

    #[must_use]
    pub fn statements(&self) -> Vec<CSTStatement<'a>> {
        self.children().filter_map(CSTStatement::cast).collect()
    }

    #[must_use]
    pub fn collect_semantic_tokens(&self) -> Vec<SemanticToken> {
        let mut tokens = Vec::new();

        for statement in self.statements() {
            statement.collect_semantic_tokens(&mut tokens);
        }

        tokens.sort_by_key(|t| t.span.start);

        tokens
    }
}
