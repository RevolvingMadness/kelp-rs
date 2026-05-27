use kelp_core::{parsed::program::Program, trait_ext::CollectOptionAllIterExt};

use crate::{
    cst::{CSTItem, CSTProgram},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTProgram {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::Program);

        loop {
            parser.skip_whitespace();

            if parser.is_eof() {
                break;
            }

            CSTItem::expect(parser, "Expected item");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTProgram {
    type Lowered = Program;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let items = self
            .items()
            .map(|item| item.lower(ctx))
            .collect_option_all()?;

        Some(Program { items })
    }
}
