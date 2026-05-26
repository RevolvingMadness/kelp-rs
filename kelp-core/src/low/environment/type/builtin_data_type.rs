use crate::semantic::environment::r#type::builtin_data_type::BuiltinTypeKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinTypeId(pub u32);

#[derive(Debug, Clone)]
pub struct BuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}
