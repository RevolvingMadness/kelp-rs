use kelp_core::span::Span;

use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy)]
pub struct CSTToken {
    pub kind: SyntaxKind,
    pub span: Span,
}

impl CSTToken {
    #[inline]
    #[must_use]
    pub fn as_tuple(self) -> (SyntaxKind, Span) {
        (self.kind, self.span)
    }

    #[inline]
    #[must_use]
    pub fn text<'a>(&self, text: &'a str) -> &'a str {
        &text[self.span.into_range()]
    }
}
