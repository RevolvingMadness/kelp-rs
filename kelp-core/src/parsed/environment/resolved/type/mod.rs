use crate::{
    parsed::environment::resolved::r#type::{
        alias::ResolvedTypeAliasDeclaration,
        builtin_data_type::ResolvedBuiltinTypeDeclaration,
        module::{HighModuleId, ResolvedModuleDeclaration},
        r#struct::{
            HighStructId, ResolvedStructDeclaration, regular::HighRegularStructId,
            tuple::HighTupleStructId,
        },
    },
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighGenericId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub u32);

impl From<HighModuleId> for HighTypeId {
    fn from(value: HighModuleId) -> Self {
        Self(value.0)
    }
}

impl From<HighStructId> for HighTypeId {
    fn from(value: HighStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularStructId> for HighTypeId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighTupleStructId> for HighTypeId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighGenericId> for HighTypeId {
    fn from(value: HighGenericId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedTypeDeclarationKind {
    Module(ResolvedModuleDeclaration),
    Struct(ResolvedStructDeclaration),
    Alias(ResolvedTypeAliasDeclaration),
    Generic(String),
    Builtin(ResolvedBuiltinTypeDeclaration),
}

impl ResolvedTypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Generic(name) => name,
            Self::Builtin(data_type) => &data_type.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: ResolvedTypeDeclarationKind,
}
