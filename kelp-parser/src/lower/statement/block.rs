use crate::{cst_node, lower::statement::CSTStatement, parser::Parser, syntax::SyntaxKind};

cst_node!(CSTBlockStatement, SyntaxKind::BlockStatement);

impl<'a> CSTBlockStatement<'a> {
    pub fn statements(&self) -> impl Iterator<Item = CSTStatement<'a>> {
        self.children().filter_map(CSTStatement::cast)
    }

    pub fn try_parse(parser: &mut Parser) -> bool {
        if !parser.expect_no_bump('{', "Expected '{'") {
            return false;
        }

        parser.start_node(SyntaxKind::BlockStatement);
        parser.bump_char();

        let mut is_first = true;

        parser.skip_whitespace();

        while !parser.is_eof() && parser.peek_char() != Some('}') {
            if !is_first
                && !parser.expect_newline_whitespace("Expected newline to mark end of statement")
            {
                let chars = parser.source[parser.pos..].chars();
                let mut length = 0;

                for char in chars {
                    if CSTStatement::is_recovery(char) || char == '}' {
                        break;
                    }

                    length += char.len_utf8();
                }

                parser.add_token(SyntaxKind::Garbage, length);
            }

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            if !CSTStatement::try_parse(parser) {
                parser.recover_newline("Expected statement");
            }

            is_first = false;
        }

        parser.skip_whitespace();

        parser.expect_char('}', "Expected '}'");
        parser.finish_node();

        true
    }
}
