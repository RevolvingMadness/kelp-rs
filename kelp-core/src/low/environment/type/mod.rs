use crate::low::environment::r#type::r#struct::StructDeclaration;
use crate::make_id;

pub mod builtin_data_type;
pub mod r#struct;

make_id!(TypeId);

#[derive(Debug, Clone)]
pub enum TypeDeclaration {
    Struct(StructDeclaration),
}

impl TypeDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => declaration.name(),
        }
    }
}
