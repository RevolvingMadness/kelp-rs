use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTStringExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTStringExpression {
    type Lowered = ParsedExpression;

    fn lower(self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let text_token = self.string_literal_token()?;
        let string = text_token.text().trim_matches('"');

        Some(ParsedExpressionKind::String(string.to_owned()).with_span(self.span()))
    }
}
