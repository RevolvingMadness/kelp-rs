use crate::{data_type::DataTypeKind, datapack::DataTypeDeclarationKind};

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>>;

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>>;
}
