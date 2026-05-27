use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::{CSTDataExpression, CSTDataTarget, CSTNBTPath},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTDataExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTDataTarget::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::DataExpression);

        parser.expect_inline_whitespace();

        CSTNBTPath::expect(parser, "Expected nbt path");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTDataExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = self.data_target()?.lower(ctx)?;
        let path = self.nbt_path()?.lower(ctx)?;

        Some(ParsedExpressionKind::Data(Box::new((target, path))).with_span(self.span()))
    }
}
