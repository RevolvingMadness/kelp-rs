use crate::{make_id, semantic::environment::r#type::builtin_data_type::BuiltinTypeKind};

make_id!(BuiltinTypeId);

#[derive(Debug, Clone)]
pub struct BuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}
