use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTAsCastExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTAsCastExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expression = self.expression()?.lower(ctx)?;
        let data_type = self.data_type()?.lower(ctx)?;

        Some(ParsedExpressionKind::AsCast(Box::new(expression), data_type).with_span(self.span()))
    }
}
