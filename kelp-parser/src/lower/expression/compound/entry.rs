use kelp_core::span::Span;

use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(
    CSTCompoundExpressionEntry,
    SyntaxKind::CompoundExpressionEntry
);

impl<'a> CSTCompoundExpressionEntry<'a> {
    pub(crate) fn key(&self) -> Option<(Span, String)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier || token.kind == SyntaxKind::String {
                if token.kind == SyntaxKind::String {
                    Some((token.span, token.text.trim_matches('"').to_string()))
                } else {
                    Some((token.span, token.text.to_string()))
                }
            } else {
                None
            }
        })
    }

    pub(crate) fn value(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }
}
