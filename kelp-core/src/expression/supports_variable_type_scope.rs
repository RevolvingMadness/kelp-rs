use crate::{
    data_type::DataTypeKind, datapack::DataTypeDeclarationKind,
    semantic_analysis_context::SemanticAnalysisInfo,
};

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>>;

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>>;

    fn add_info(&mut self, semantic_analysis_info: SemanticAnalysisInfo);
}
