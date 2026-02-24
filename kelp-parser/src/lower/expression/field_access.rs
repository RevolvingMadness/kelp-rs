use kelp_core::span::Span;

use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTFieldAccessExpression, SyntaxKind::FieldAccessExpression);

impl<'a> CSTFieldAccessExpression<'a> {
    pub fn target(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn field<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier || token.kind == SyntaxKind::WholeValue {
                Some((token.span, token.text(text)))
            } else {
                None
            }
        })
    }
}
