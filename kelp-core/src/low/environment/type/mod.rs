use crate::{
    builtin_data_type::BuiltinDataType, low::environment::r#type::r#struct::StructDeclaration,
    visibility::Visibility,
};

pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone)]
pub enum TypeDeclarationKind {
    Struct(StructDeclaration),
    Builtin(BuiltinDataType),
}

impl TypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => declaration.name(),
            Self::Builtin(data_type) => data_type.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: TypeDeclarationKind,
}

impl TypeDeclaration {
    #[inline]
    #[must_use]
    pub fn as_tuple(&self) -> (Visibility, &[String], &TypeDeclarationKind) {
        (self.visibility, &self.module_path, &self.kind)
    }
}
