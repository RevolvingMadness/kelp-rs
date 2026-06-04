use crate::low::environment::r#type::r#struct::StructDeclaration;
use crate::make_id;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::visibility::Visibility;

pub mod builtin_data_type;
pub mod r#struct;

make_id!(TypeId);

#[derive(Debug, Clone)]
pub enum TypeDeclarationKind {
    Struct(StructDeclaration),
}

impl TypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<HighModuleId>,
    pub kind: TypeDeclarationKind,
}
