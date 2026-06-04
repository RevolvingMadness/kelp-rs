use std::collections::HashMap;

use crate::{
    make_id,
    parsed::semantic_analysis::info::error::SemanticAnalysisError,
    semantic::environment::{
        SemanticEnvironment,
        r#type::{HighTypeId, HighVisibleTypeId},
        value::{HighValueId, HighVisibleValueId},
    },
    span::Span,
};

make_id!(HighModuleId);

impl From<HighModuleId> for HighTypeId {
    fn from(value: HighModuleId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticModuleDeclaration {
    pub name_span: Option<Span>,
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl SemanticModuleDeclaration {
    #[must_use]
    fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[must_use]
    fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }

    pub fn get_visible_type_id(
        &self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        name: &str,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        let Some(id) = self.get_type_id(name) else {
            return Err(SemanticAnalysisError::TypeDoesntContainType {
                container_type_name: self.name.clone(),
                type_name: name.to_owned(),
            });
        };

        let declaration = semantic_environment.get_type(id);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::TypeNotPublic(name.to_owned()));
        }

        Ok(HighVisibleTypeId(id.0))
    }

    pub fn get_visible_value_id(
        &self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        name: &str,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        let Some(id) = self.get_value_id(name) else {
            return Err(SemanticAnalysisError::TypeDoesntContainValue {
                type_name: self.name.clone(),
                value_name: name.to_owned(),
            });
        };

        let declaration = semantic_environment.get_value(id);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::TypeNotPublic(name.to_owned()));
        }

        Ok(HighVisibleValueId(id.0))
    }
}
