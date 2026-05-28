use crate::parsed::environment::r#type::r#struct::{
    regular::ParsedRegularStructDeclaration, tuple::ParsedTupleStructDeclaration,
};
use crate::parsed::semantic_analysis::{
    SemanticAnalysisContext, info::error::SemanticAnalysisError,
};
use crate::path::generic::GenericPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::{
    r#type::{HighTypeId, r#struct::SemanticStructDeclaration},
    value::HighValueId,
};

pub mod regular;
pub mod tuple;

#[derive(Debug, Clone)]
pub enum ParsedStructDeclaration {
    Struct(ParsedRegularStructDeclaration),
    Tuple(ParsedTupleStructDeclaration),
}

impl From<SemanticStructDeclaration> for ParsedStructDeclaration {
    fn from(value: SemanticStructDeclaration) -> Self {
        match value {
            SemanticStructDeclaration::Struct(declaration) => Self::Struct(declaration.into()),
            SemanticStructDeclaration::Tuple(declaration) => Self::Tuple(declaration.into()),
        }
    }
}

impl ParsedStructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => declaration.name(),
            Self::Tuple(declaration) => declaration.name(),
        }
    }

    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_count(),
            Self::Tuple(declaration) => declaration.generic_count(),
        }
    }

    pub fn get_value_id_semantic_analysis(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighValueId, SemanticAnalysisError> {
        if let Some(impls) = ctx.semantic_environment.implementations.get(&id) {
            for implementation in impls {
                if let Some(id) = implementation.values.get(name) {
                    return Ok(*id);
                }
            }
        }

        Err(SemanticAnalysisError::TypeDoesntContainValue {
            type_name: self.name().to_owned(),
            value_name: name.to_owned(),
        })
    }

    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        let id = HighStructId(id.0);

        let expected_generics = self.generic_count();
        let actual_generics = segment.generic_types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics_type(
                segment.name_span,
                self.name(),
                expected_generics,
                actual_generics,
            );
        }

        SemanticDataType::Struct(id, segment.generic_types.clone())
    }
}
