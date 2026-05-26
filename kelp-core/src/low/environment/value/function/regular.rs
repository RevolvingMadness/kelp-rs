use la_arena::Idx;

use crate::{
    low::environment::value::function::{FunctionDeclaration, FunctionId},
    parsed::semantic_analysis::RegularFunctionModifiers,
    typed::{
        data_type::resolved::DataType, environment::r#type::HighGenericId,
        expression::TypedExpressionId, pattern::TypedPattern,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFunctionId(pub u32);

impl From<RegularFunctionId> for FunctionId {
    fn from(value: RegularFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub module_paths: Vec<String>,
    pub visibility: Visibility,
    pub modifiers: RegularFunctionModifiers,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<(Idx<TypedPattern>, DataType)>,
    pub return_type: DataType,
    pub body: TypedExpressionId,
}

impl From<RegularFunctionDeclaration> for FunctionDeclaration {
    fn from(value: RegularFunctionDeclaration) -> Self {
        Self::Regular(value)
    }
}
