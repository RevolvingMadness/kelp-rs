use kelp_core::high::{program::Program, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTProgram,
    item::{expect_item, lower_item},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn parse_program(parser: &mut Parser) {
    parser.start_node(SyntaxKind::Program);

    loop {
        parser.skip_whitespace();

        if parser.is_eof() {
            break;
        }

        expect_item(parser);
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_program(program: &CSTProgram, ctx: &mut SemanticAnalysisContext) -> Program {
    let items = program
        .items()
        .filter_map(|item| lower_item(item, ctx))
        .collect();

    Program { items }
}
