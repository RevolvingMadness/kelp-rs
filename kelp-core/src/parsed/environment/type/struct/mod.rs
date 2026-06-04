use crate::parsed::environment::r#type::r#struct::{
    regular::ParsedRegularStructDeclaration, tuple::ParsedTupleStructDeclaration,
};
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighVisibleTypeId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::r#struct::SemanticStructDeclaration;
use crate::span::Span;

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
    pub const fn name_span(&self) -> Span {
        match self {
            Self::Struct(declaration) => declaration.name_span,
            Self::Tuple(declaration) => declaration.name_span,
        }
    }

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

    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighVisibleTypeId,
        name_span: Span,
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        let id = HighStructId(id.0);

        let expected_generics = self.generic_count();
        let actual_generics = generic_types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics_type(
                name_span,
                self.name(),
                expected_generics,
                actual_generics,
            );
        }

        SemanticDataType::Struct(id, generic_types.to_vec())
    }
}
