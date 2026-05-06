use strum::EnumIter;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::environment::value::function::builtin::HighBuiltinFunctionDeclaration,
    low::{
        data_type::{resolved::ResolvedDataType, unresolved::UnresolvedDataType},
        environment::value::function::{
            builtin::{BuiltinFunctionDeclaration, BuiltinFunctionId},
            regular::{RegularFunctionDeclaration, RegularFunctionId},
        },
        expression::resolved::ResolvedExpression,
        pattern::UnresolvedPattern,
    },
    parameter_types_iter::{ParameterTypesIter, take_second},
};

pub mod builtin;
pub mod regular;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub usize);

impl From<RegularFunctionId> for FunctionId {
    fn from(value: RegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<BuiltinFunctionId> for FunctionId {
    fn from(value: BuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum FunctionDeclaration {
    Regular(RegularFunctionDeclaration),
    Builtin(BuiltinFunctionDeclaration),
}

impl From<RegularFunctionDeclaration> for FunctionDeclaration {
    fn from(value: RegularFunctionDeclaration) -> Self {
        Self::Regular(value)
    }
}

impl From<BuiltinFunctionDeclaration> for FunctionDeclaration {
    fn from(value: BuiltinFunctionDeclaration) -> Self {
        Self::Builtin(value)
    }
}

impl FunctionDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => &declaration.name,
            Self::Builtin(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_types(&self) -> &[ResolvedDataType] {
        match self {
            Self::Regular(declaration) => &declaration.generic_types,
            Self::Builtin(declaration) => &declaration.generic_types,
        }
    }

    #[must_use]
    pub fn parameter_types(&self) -> ParameterTypesIter<'_, UnresolvedPattern, ResolvedDataType> {
        match self {
            Self::Regular(declaration) => {
                ParameterTypesIter::Regular(declaration.parameters.iter().map(take_second))
            }
            Self::Builtin(declaration) => {
                ParameterTypesIter::Builtin(declaration.parameters.iter())
            }
        }
    }

    #[must_use]
    pub const fn return_type(&self) -> &ResolvedDataType {
        match self {
            Self::Regular(declaration) => &declaration.return_type,
            Self::Builtin(declaration) => &declaration.return_type,
        }
    }
}

#[derive(Debug, Clone, Copy, EnumIter)]
pub enum BuiltinFunctionKind {
    StdAdd, // fn add(integer, integer) -> integer
}

impl BuiltinFunctionKind {
    #[must_use]
    pub fn declaration(self) -> HighBuiltinFunctionDeclaration {
        match self {
            Self::StdAdd => HighBuiltinFunctionDeclaration {
                name: "add".to_owned(),
                generic_names: Vec::new(),
                parameters: vec![UnresolvedDataType::Integer, UnresolvedDataType::Integer],
                return_type: UnresolvedDataType::Integer,
                kind: self,
            },
        }
    }

    pub fn call(
        self,
        _datapack: &mut Datapack,
        _ctx: &mut CompileContext,
        mut arguments: Vec<ResolvedExpression>,
    ) -> ResolvedExpression {
        match self {
            Self::StdAdd => {
                let ResolvedExpression::Integer(a) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                let ResolvedExpression::Integer(b) = arguments.pop().unwrap() else {
                    unreachable!();
                };

                ResolvedExpression::Integer(a + b)
            }
        }
    }
}
