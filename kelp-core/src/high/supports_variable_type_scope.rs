use crate::middle::{data_type::DataType, data_type_declaration::DataTypeDeclarationKind};

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataType>>;

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>>;
}
