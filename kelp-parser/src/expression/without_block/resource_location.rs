use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTResourceLocation, CSTResourceLocationExpression},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTResourceLocationExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::ResourceLocationExpression);

        parser.bump_identifier_kind(SyntaxKind::ResourceLocationKeyword, "resource_location");

        parser.skip_whitespace();

        if !parser.try_bump_char(':') {
            state.restore(parser);

            return false;
        }

        parser.skip_whitespace();

        if !CSTResourceLocation::try_parse(parser) {
            parser.error("Expected resource location");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTResourceLocationExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let resource_location = self.resource_location()?.lower(ctx)?;

        Some(
            ParsedExpressionKind::ResourceLocation(Box::new(resource_location))
                .with_span(self.span()),
        )
    }
}
