use kelp_core::span::Span;

use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy)]
pub struct CSTToken<'a> {
    pub kind: SyntaxKind,
    pub text: &'a str,
    pub span: Span,
}
