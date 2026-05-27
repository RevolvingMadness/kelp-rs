use kelp_core::parsed::data::Data;

use crate::{
    cst::{CSTData, CSTDataTarget, CSTNBTPath},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod nbt_path;
pub mod target;

impl ParsableAstNode for CSTData {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTDataTarget::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::Data);

        parser.expect_inline_whitespace();

        CSTNBTPath::expect(parser, "Expected nbt path");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTData {
    type Lowered = Data;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = self.data_target()?.lower(ctx)?;
        let path = self.nbt_path()?.lower(ctx)?;

        Some(Data { target, path })
    }
}
