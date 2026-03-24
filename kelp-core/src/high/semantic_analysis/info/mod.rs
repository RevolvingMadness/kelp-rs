use crate::{high::semantic_analysis::info::error::SemanticAnalysisError, span::Span};

pub mod error;

#[derive(Debug, Clone)]
pub enum SemanticAnalysisInfoKind {
    // Warning(SemanticAnalysisWarning),
    Error(SemanticAnalysisError),
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisInfo {
    pub span: Span,
    pub kind: SemanticAnalysisInfoKind,
}
