use std::{iter::Map, slice::Iter};

use strum::EnumIter;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::environment::value::function::builtin::HighBuiltinFunctionDeclaration,
    low::{
        data_type::DataType,
        environment::value::function::{
            builtin::{BuiltinFunctionDeclaration, BuiltinFunctionId},
            regular::{RegularFunctionDeclaration, RegularFunctionId},
        },
        expression::resolved::ResolvedExpression,
        pattern::Pattern,
    },
};

pub mod builtin;
pub mod regular;

type TakeDataTypeFn = fn(&(Pattern, DataType)) -> &DataType;

pub enum ParametersIter<'a> {
    Regular(Map<Iter<'a, (Pattern, DataType)>, TakeDataTypeFn>),
    Builtin(Iter<'a, DataType>),
}

impl<'a> Iterator for ParametersIter<'a> {
    type Item = &'a DataType;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Regular(iterator) => iterator.next(),
            Self::Builtin(iterator) => iterator.next(),
        }
    }
}

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
    pub fn generic_types(&self) -> &[DataType] {
        match self {
            Self::Regular(declaration) => &declaration.generic_types,
            Self::Builtin(declaration) => &declaration.generic_types,
        }
    }

    #[must_use]
    pub fn parameters(&self) -> ParametersIter<'_> {
        const fn take_data_type((_, data_type): &(Pattern, DataType)) -> &DataType {
            data_type
        }

        match self {
            Self::Regular(declaration) => {
                ParametersIter::Regular(declaration.parameters.iter().map(take_data_type))
            }
            Self::Builtin(declaration) => ParametersIter::Builtin(declaration.parameters.iter()),
        }
    }

    #[must_use]
    pub const fn return_type(&self) -> &DataType {
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
                parameters: vec![DataType::Integer, DataType::Integer],
                return_type: DataType::Integer,
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
