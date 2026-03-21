use crate::{
    builtin_data_type::BuiltinDataType, middle::environment::r#type::r#struct::StructDeclaration,
};

pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone)]
pub enum TypeDeclaration {
    Struct(StructDeclaration),
    Builtin(BuiltinDataType),
}

impl TypeDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Builtin(data_type) => data_type.name(),
        }
    }
}
