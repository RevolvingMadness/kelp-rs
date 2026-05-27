use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTUnderscoreExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTUnderscoreExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedExpressionKind::Underscore.with_span(self.span()))
    }
}
