use kelp_core::parsed::program::Program;

use crate::{
    cst::{CSTItem, CSTProgram},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
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

        CSTItem::expect(parser, "Expected item");
    }

    parser.finish_node();
}

#[must_use]
pub fn lower_program(program: &CSTProgram, ctx: &mut LowerContext) -> Program {
    let items = program.items().filter_map(|item| item.lower(ctx)).collect();

    Program { items }
}
