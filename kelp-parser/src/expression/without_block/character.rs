use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTCharacterExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTCharacterExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let character_token = self.character_literal_token()?;
        let character = character_token.text().trim_matches('\'');

        Some(ParsedExpressionKind::String(character.to_owned()).with_span(self.span()))
    }
}
