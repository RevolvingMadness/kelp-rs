use kelp_core::span::Span;

#[derive(Debug, Clone)]
pub struct CSTError {
    pub message: String,
    pub span: Span,
}
