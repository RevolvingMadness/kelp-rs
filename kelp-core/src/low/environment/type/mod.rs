use crate::visibility::Visibility;
use crate::low::environment::r#type::r#struct::StructDeclaration;

pub mod builtin_data_type;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub u32);

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
    pub module_path: Vec<String>,
    pub kind: TypeDeclarationKind,
}
