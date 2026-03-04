use kelp_core::span::Span;
use rowan::{Language, SyntaxToken, TextRange, ast::AstNode};

#[must_use]
#[inline]
pub fn text_range_to_span(range: TextRange) -> Span {
    Span {
        start: range.start().into(),
        end: range.end().into(),
    }
}

#[must_use]
#[inline]
pub fn span_of_cst_node(node: &impl AstNode) -> Span {
    text_range_to_span(node.syntax().text_range())
}

#[must_use]
#[inline]
pub fn span_of_syntax_token<T: Language>(token: &SyntaxToken<T>) -> Span {
    text_range_to_span(token.text_range())
}
