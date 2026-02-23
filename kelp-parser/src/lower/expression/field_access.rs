use kelp_core::span::Span;

use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTFieldAccessExpression, SyntaxKind::FieldAccessExpression);

impl<'a> CSTFieldAccessExpression<'a> {
    pub fn target(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn field(&self) -> Option<(Span, &'a str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier || token.kind == SyntaxKind::Integer {
                Some((token.span, token.text))
            } else {
                None
            }
        })
    }
}
