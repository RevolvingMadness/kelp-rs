use la_arena::Idx;

use crate::{
    parsed::{
        environment::resolved::r#type::HighGenericId, semantic_analysis::RegularFunctionModifiers,
    },
    typed::{
        data_type::resolved::DataType, expression::typed::TypedExpressionId,
        pattern::TypedPattern,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFunctionId(pub u32);

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
