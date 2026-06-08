use crate::parsed::environment::r#type::r#struct::unit::ParsedUnitStructDeclaration;
use crate::parsed::environment::r#type::r#struct::{
    regular::ParsedRegularStructDeclaration, tuple::ParsedTupleStructDeclaration,
};
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, TypeKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighVisibleTypeId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::r#struct::SemanticStructDeclaration;
use crate::span::Span;

pub mod regular;
pub mod tuple;
pub mod unit;

#[derive(Debug, Clone)]
pub enum ParsedStructDeclaration {
    Regular(ParsedRegularStructDeclaration),
    Tuple(ParsedTupleStructDeclaration),
    Unit(ParsedUnitStructDeclaration),
}

impl From<SemanticStructDeclaration> for ParsedStructDeclaration {
    fn from(value: SemanticStructDeclaration) -> Self {
        match value {
            SemanticStructDeclaration::Regular(declaration) => Self::Regular(declaration.into()),
            SemanticStructDeclaration::Tuple(declaration) => Self::Tuple(declaration.into()),
            SemanticStructDeclaration::Unit(declaration) => Self::Unit(declaration.into()),
        }
    }
}

impl ParsedStructDeclaration {
    #[must_use]
    pub const fn name_span(&self) -> Span {
        match self {
            Self::Regular(declaration) => declaration.name_span,
            Self::Tuple(declaration) => declaration.name_span,
            Self::Unit(declaration) => declaration.name_span,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Regular(declaration) => declaration.name(),
            Self::Tuple(declaration) => declaration.name(),
            Self::Unit(declaration) => declaration.name(),
        }
    }

    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Regular(declaration) => declaration.generic_count(),
            Self::Tuple(declaration) => declaration.generic_count(),
            Self::Unit(..) => 0,
        }
    }

    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighVisibleTypeId,
        name_span: Span,
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        let id = HighStructId(id.0);

        let expected_generic_count = self.generic_count();
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                type_name_span: name_span,
                item_kind: TypeKind::Struct.into(),
                declaration_span: Some(self.name_span()),
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        SemanticDataType::Struct(id, generic_types.to_vec())
    }
}
