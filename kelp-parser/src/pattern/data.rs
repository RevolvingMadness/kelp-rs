use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::{CSTData, CSTDataPattern},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTDataPattern {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTData::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::DataPattern);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTDataPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let data = self.data()?.lower(ctx)?;

        Some(ParsedPatternKind::Data(Box::new(data)).with_span(self.span()))
    }
}
