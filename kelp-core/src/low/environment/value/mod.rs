use crate::{
    low::{data_type::DataType, environment::value::variable::VariableDeclaration},
    visibility::Visibility,
};

pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub usize);

#[derive(Debug, Clone)]
pub enum ValueDeclarationKind {
    Variable(VariableDeclaration),
}

impl ValueDeclarationKind {
    #[inline]
    #[must_use]
    pub const fn with_visibility(
        self,
        module_path: Vec<String>,
        visibility: Visibility,
    ) -> ValueDeclaration {
        ValueDeclaration {
            visibility,
            module_path,
            kind: self,
        }
    }

    #[inline]
    #[must_use]
    pub const fn public(self, module_path: Vec<String>) -> ValueDeclaration {
        self.with_visibility(module_path, Visibility::Public)
    }

    #[inline]
    #[must_use]
    pub const fn none_visibility(self, module_path: Vec<String>) -> ValueDeclaration {
        self.with_visibility(module_path, Visibility::None)
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub const fn data_type(&self) -> Option<&DataType> {
        match self {
            Self::Variable(declaration) => declaration.data_type.as_ref(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: ValueDeclarationKind,
}

impl ValueDeclaration {
    #[inline]
    #[must_use]
    pub fn as_tuple(&self) -> (Visibility, &[String], &ValueDeclarationKind) {
        (self.visibility, &self.module_path, &self.kind)
    }
}
