use std::collections::HashMap;
use crate::semantic::environment::{
    r#type::{module::ResolvedModuleDeclaration, HighTypeId},
    value::HighValueId,
};
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;

#[derive(Debug, Clone)]
pub struct ParsedModuleDeclaration {
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl From<ResolvedModuleDeclaration> for ParsedModuleDeclaration {
    fn from(value: ResolvedModuleDeclaration) -> Self {
        Self {
            name: value.name,
            types: value.types,
            values: value.values,
        }
    }
}

impl ParsedModuleDeclaration {
    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    pub fn get_type_id_semantic_analysis(
        &self,
        name: &str,
    ) -> Result<HighTypeId, SemanticAnalysisError> {
        self.get_type_id(name)
            .ok_or_else(|| SemanticAnalysisError::TypeDoesntContainType {
                container_type_name: self.name.clone(),
                type_name: name.to_owned(),
            })
    }

    pub fn get_value_id_semantic_analysis(
        &self,
        name: &str,
    ) -> Result<HighValueId, SemanticAnalysisError> {
        self.get_value_id(name)
            .ok_or_else(|| SemanticAnalysisError::TypeDoesntContainValue {
                type_name: self.name.clone(),
                value_name: name.to_owned(),
            })
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }
}
