use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    runtime_storage::RuntimeStorageType,
};

use crate::{
    cst::CSTToCastExpression,
    extension_traits::{AstNodeExt, LowerableAstNode, SyntaxTokenExt},
    lower_context::{LowerContext, LowerError},
};

impl LowerableAstNode for CSTToCastExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expression = self.expression()?.lower(ctx)?;
        let runtime_storage_type_token = self.runtime_storage_type_token()?;
        let runtime_storage_type_span = runtime_storage_type_token.span();
        let runtime_storage_type_text = runtime_storage_type_token.text();

        let runtime_storage_type = match runtime_storage_type_text {
            "data" => RuntimeStorageType::Data,
            "score" => RuntimeStorageType::Score,
            _ => {
                ctx.add_error_unit(LowerError::InvalidRuntimeStorageType {
                    span: runtime_storage_type_span,
                });

                return Some(ParsedExpressionKind::Invalid.with_span(self.span()));
            }
        };

        Some(
            ParsedExpressionKind::ToCast(Box::new(expression), runtime_storage_type)
                .with_span(self.span()),
        )
    }
}
