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
