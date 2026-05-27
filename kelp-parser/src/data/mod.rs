use kelp_core::parsed::data::Data;

use crate::{
    cst::CSTData,
    data::{
        nbt_path::{lower_nbt_path, try_parse_nbt_path},
        target::{lower_data_target, try_parse_data_target},
    },
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

        if !try_parse_data_target(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::Data);

        parser.expect_inline_whitespace();

        if !try_parse_nbt_path(parser) {
            parser.error("Expected nbt path");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTData {
    type Lowered = Data;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = lower_data_target(self.data_target()?, ctx)?;
        let path = lower_nbt_path(self.nbt_path()?, ctx)?;

        Some(Data { target, path })
    }
}
