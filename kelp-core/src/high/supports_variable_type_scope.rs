use crate::middle::data_type_declaration::TypeDeclaration;

pub trait SupportsVariableTypeScope {
    fn get_data_type(&self, name: &str) -> Option<Option<TypeDeclaration>>;
}
