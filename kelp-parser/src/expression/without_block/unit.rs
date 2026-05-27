use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTUnitExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTUnitExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedExpressionKind::Unit.with_span(self.span()))
    }
}
