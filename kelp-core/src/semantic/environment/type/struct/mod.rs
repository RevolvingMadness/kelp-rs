use crate::make_id;
use crate::parsed::environment::ParsedEnvironment;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, TypeKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::r#type::r#struct::unit::SemanticUnitStructDeclaration;
use crate::semantic::environment::r#type::{
    HighGenericId,
    r#struct::{regular::SemanticRegularStructDeclaration, tuple::SemanticTupleStructDeclaration},
};
use crate::semantic::environment::r#type::{HighTypeId, HighVisibleTypeId};
use crate::semantic::environment::value::HighVisibleValueId;
use crate::span::Span;

pub mod regular;
pub mod tuple;
pub mod unit;

make_id!(HighStructId);

impl From<HighStructId> for HighTypeId {
    fn from(value: HighStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum SemanticStructDeclaration {
    Struct(SemanticRegularStructDeclaration),
    Tuple(SemanticTupleStructDeclaration),
    Unit(SemanticUnitStructDeclaration),
}

impl SemanticStructDeclaration {
    #[must_use]
    pub const fn name_span(&self) -> Span {
        match self {
            Self::Struct(declaration) => declaration.name_span,
            Self::Tuple(declaration) => declaration.name_span,
            Self::Unit(declaration) => declaration.name_span,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
            Self::Unit(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        match self {
            Self::Struct(declaration) => &declaration.generic_ids,
            Self::Tuple(declaration) => &declaration.generic_ids,
            Self::Unit(..) => &[],
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_ids.len(),
            Self::Tuple(declaration) => declaration.generic_ids.len(),
            Self::Unit(..) => 0,
        }
    }

    pub fn get_visible_type_id(
        &self,
        parsed_environment: &ParsedEnvironment,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        if let Some(impls) = semantic_environment.get_implementations(self_id) {
            for implementation in impls {
                if let Some(id) = implementation.get_type(name) {
                    return id.assert_visible_result(parsed_environment, current_module_path);
                }
            }
        }

        Err(SemanticAnalysisError::TypeDoesntContainType {
            container_type_declaration_span: Some(self.name_span()),
            container_type_kind: TypeKind::Struct,
            container_type_name: self.name().to_owned(),
            type_span: name_span,
            type_name: name.to_owned(),
        })
    }

    pub fn get_visible_value_id(
        &self,
        parsed_environment: &ParsedEnvironment,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        if let Some(implementations) = semantic_environment.get_implementations(self_id) {
            for implementation in implementations {
                if let Some(id) = implementation.get_value(name) {
                    return id.assert_visible_result(parsed_environment, current_module_path);
                }
            }
        }

        Err(SemanticAnalysisError::TypeDoesntContainValue {
            type_declaration_span: Some(self.name_span()),
            type_kind: TypeKind::Struct,
            type_name: self.name().to_owned(),
            value_span: name_span,
            value_name: name.to_owned(),
        })
    }

    #[must_use]
    pub fn get_field(&self, field_name: &str) -> Option<&SemanticDataType> {
        match self {
            Self::Struct(declaration) => declaration.field_types.get(field_name),
            Self::Tuple(declaration) => {
                let field_index = field_name.parse::<usize>().ok()?;

                declaration.field_types.get(field_index)
            }
            Self::Unit(declaration) => None,
        }
    }
}
