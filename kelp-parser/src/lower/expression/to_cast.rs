use kelp_core::span::Span;

use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTToCastExpression, SyntaxKind::ToCastExpression);

impl<'a> CSTToCastExpression<'a> {
    #[must_use]
    pub fn expression(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    #[must_use]
    pub fn to_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn runtime_storage_type_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn runtime_storage_type<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }
}
