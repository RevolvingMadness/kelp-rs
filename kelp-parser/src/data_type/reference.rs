use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::{CSTDataType, CSTReferenceDataType},
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTReferenceDataType {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();
        marker.start_node(parser, SyntaxKind::ReferenceDataType);

        parser.bump_char();

        if !CSTDataType::try_parse(parser) {
            parser.recover_not_whitespace("Expected data type after '&'");
        }

        parser.finish_node();
        true
    }
}

impl LowerableAstNode for CSTReferenceDataType {
    type Lowered = ParsedDataType;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let data_type = self.data_type()?.lower(ctx)?;

        Some(ParsedDataType::Reference(Box::new(data_type)))
    }
}
