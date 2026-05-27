use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::{CSTPathDataType, CSTTypePath},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTPathDataType {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTTypePath::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::PathDataType);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTPathDataType {
    type Lowered = ParsedDataType;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = self.type_path()?.lower(ctx)?;

        Some(ParsedDataType::Named(path))
    }
}
