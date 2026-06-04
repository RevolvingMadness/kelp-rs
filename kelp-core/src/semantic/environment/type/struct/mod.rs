use crate::make_id;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::{
    HighGenericId,
    r#struct::{regular::SemanticRegularStructDeclaration, tuple::SemanticTupleStructDeclaration},
};
use crate::semantic::environment::r#type::{HighTypeId, HighVisibleTypeId};
use crate::semantic::environment::value::HighVisibleValueId;

pub mod regular;
pub mod tuple;

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
}

impl SemanticStructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
        }
    }

    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        match self {
            Self::Struct(declaration) => &declaration.generic_ids,
            Self::Tuple(declaration) => &declaration.generic_ids,
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_ids.len(),
            Self::Tuple(declaration) => declaration.generic_ids.len(),
        }
    }

    pub fn get_visible_value_id(
        &self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[String],
        id: HighVisibleTypeId,
        name: &str,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        if let Some(implementations) = semantic_environment.get_implementations(id) {
            for implementation in implementations {
                if let Some(id) = implementation.get_value(name) {
                    return id.assert_visible_result(semantic_environment, current_module_path);
                }
            }
        }

        Err(SemanticAnalysisError::TypeDoesntContainValue {
            type_name: self.name().to_owned(),
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
        }
    }
}
