use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;

pub mod diagnostic;
pub mod error;

#[derive(Debug, Clone)]
pub enum SemanticAnalysisInfo {
    // Warning(SemanticAnalysisWarning),
    Error(SemanticAnalysisError),
}
