use crate::low::data_type::DataType;
use crate::make_id;
use crate::semantic::environment::r#type::generic::HighGenericId;
use crate::{
    parsed::semantic_analysis::RegularFunctionModifiers,
    semantic::{expression::SemanticExpression, pattern::SemanticPattern},
};

make_id!(RegularFunctionId);

#[derive(Debug, Clone)]
pub struct RegularFunctionDeclaration {
    pub modifiers: RegularFunctionModifiers,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub generic_types: Vec<DataType>,
    pub parameters: Vec<(SemanticPattern, DataType)>,
    pub return_type: DataType,
    pub body: SemanticExpression,
}
