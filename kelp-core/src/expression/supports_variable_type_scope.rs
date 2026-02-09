use crate::{data_type::DataTypeKind, semantic_analysis_context::SemanticAnalysisInfo};

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>>;

    fn add_info(&mut self, semantic_analysis_info: SemanticAnalysisInfo);
}
