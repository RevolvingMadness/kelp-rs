pub mod builtin;
pub mod regular;

use crate::high::environment::value::function::{
    builtin::{HighBuiltinFunctionDeclaration, HighBuiltinFunctionId},
    regular::{HighRegularFunctionDeclaration, HighRegularFunctionId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighFunctionId(pub usize);

impl From<HighRegularFunctionId> for HighFunctionId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighBuiltinFunctionId> for HighFunctionId {
    fn from(value: HighBuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum HighFunctionDeclaration {
    Regular(HighRegularFunctionDeclaration),
    Builtin(HighBuiltinFunctionDeclaration),
}
