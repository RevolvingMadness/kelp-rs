use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTBooleanExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTBooleanExpression {
    type Lowered = ParsedExpression;

    fn lower(self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedExpressionKind::Boolean(self.true_token().is_some()).with_span(self.span()))
    }
}
