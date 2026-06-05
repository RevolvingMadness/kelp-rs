use crate::{
    parsed::{
        environment::ParsedEnvironment,
        semantic_analysis::info::error::{SemanticAnalysisError, TypeKind},
    },
    semantic::environment::{
        r#type::{
            HighTypeId, HighVisibleTypeId,
            module::{HighModuleId, SemanticModuleDeclaration},
        },
        value::{HighValueId, HighVisibleValueId},
    },
    span::Span,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ParsedModuleDeclaration {
    pub name_span: Option<Span>,
    pub name: String,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

impl From<SemanticModuleDeclaration> for ParsedModuleDeclaration {
    fn from(value: SemanticModuleDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
            types: value.types,
            values: value.values,
        }
    }
}

impl ParsedModuleDeclaration {
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
        parsed_environment: &ParsedEnvironment,
        current_module_path: &[HighModuleId],
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        let Some(id) = self.get_type_id(name) else {
            return Err(SemanticAnalysisError::TypeDoesntContainType {
                container_type_declaration_span: self.name_span,
                container_type_kind: TypeKind::Module,
                container_type_name: self.name.clone(),
                type_span: name_span,
                type_name: name.to_owned(),
            });
        };

        let declaration = parsed_environment.get_type(id);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::TypeNotPublic(name.to_owned()));
        }

        Ok(HighVisibleTypeId(id.0))
    }

    pub fn get_visible_value_id(
        &self,
        parsed_environment: &ParsedEnvironment,
        current_module_path: &[HighModuleId],
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        let Some(id) = self.get_value_id(name) else {
            return Err(SemanticAnalysisError::TypeDoesntContainValue {
                type_declaration_span: self.name_span,
                type_kind: TypeKind::Module,
                type_name: self.name.clone(),
                value_span: name_span,
                value_name: name.to_owned(),
            });
        };

        let declaration = parsed_environment.get_value(id);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::TypeNotPublic(name.to_owned()));
        }

        Ok(HighVisibleValueId(id.0))
    }
}
