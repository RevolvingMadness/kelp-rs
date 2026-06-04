use crate::low::data_type::DataType;
use crate::make_id;
use crate::semantic::environment::r#type::HighGenericId;
use crate::{
    parsed::semantic_analysis::RegularFunctionModifiers,
    semantic::{expression::SemanticExpression, pattern::SemanticPattern},
    visibility::Visibility,
};

make_id!(RegularFunctionId);

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub module_paths: Vec<String>,
    pub visibility: Visibility,
    pub modifiers: RegularFunctionModifiers,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<(SemanticPattern, DataType)>,
    pub return_type: DataType,
    pub body: SemanticExpression,
}
