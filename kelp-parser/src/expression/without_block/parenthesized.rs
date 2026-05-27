use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::CSTParenthesizedExpression, extension_traits::LowerableAstNode,
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTParenthesizedExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        self.expression()?.lower(ctx)
    }
}
