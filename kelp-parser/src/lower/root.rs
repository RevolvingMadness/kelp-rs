use kelp_core::statement::Statement;

use crate::{
    cst::CSTRoot,
    lower::statement::{lower_statement, try_parse_statement},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_root(parser: &mut Parser) {
    parser.start_node(SyntaxKind::Root);

    parser.skip_whitespace();

    let mut is_first = true;

    loop {
        if parser.is_eof() {
            break;
        }

        if !is_first && !parser.try_parse_newline_whitespace() {
            parser.recover_newline("Expected newline to mark end of statement");

            is_first = false;

            continue;
        }

        let _ = try_parse_statement(parser);

        is_first = false;
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_root(root: &CSTRoot) -> Vec<Statement> {
    root.statements().filter_map(lower_statement).collect()
}
