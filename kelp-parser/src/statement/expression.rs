use kelp_core::parsed::statement::{ParsedStatement, ParsedStatementKind};

use crate::{
    cst::CSTExpressionStatement,
    expression::{
        with_block::lower_expression_with_block, without_block::lower_expression_without_block,
    },
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTExpressionStatement {
    type Lowered = ParsedStatement;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expression = if let Some(without_block) = self.expression_without_block() {
            lower_expression_without_block(without_block, ctx)?
        } else if let Some(with_block) = self.expression_with_block() {
            lower_expression_with_block(with_block, ctx)?
        } else {
            return None;
        };

        Some(ParsedStatementKind::Expression(expression).with_span(self.span()))
    }
}
