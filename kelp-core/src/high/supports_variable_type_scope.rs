use crate::middle::{data_type::DataTypeKind, data_type_declaration::DataTypeDeclarationKind};

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>>;

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>>;
}
