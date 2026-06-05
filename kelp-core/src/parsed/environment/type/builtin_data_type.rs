use crate::{
    parsed::semantic_analysis::{
        SemanticAnalysisContext,
        info::error::{SemanticAnalysisError, TypeKind},
    },
    semantic::{
        data_type::SemanticDataType,
        environment::r#type::builtin_data_type::{BuiltinTypeKind, SemanticBuiltinTypeDeclaration},
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub struct ParsedBuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

impl From<SemanticBuiltinTypeDeclaration> for ParsedBuiltinTypeDeclaration {
    fn from(value: SemanticBuiltinTypeDeclaration) -> Self {
        Self {
            name: value.name,
            generic_count: value.generic_count,
            kind: value.kind,
        }
    }
}

impl ParsedBuiltinTypeDeclaration {
    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        name_span: Span,
        generic_spans: &[Span],
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        let expected_generic_count = self.generic_count;
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                type_name_span: name_span,
                type_kind: TypeKind::Builtin.into(),
                declaration_span: None,
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        match self.kind {
            BuiltinTypeKind::Boolean => SemanticDataType::Boolean,
            BuiltinTypeKind::Byte => SemanticDataType::Byte,
            BuiltinTypeKind::Short => SemanticDataType::Short,
            BuiltinTypeKind::Integer => SemanticDataType::Integer,
            BuiltinTypeKind::Long => SemanticDataType::Long,
            BuiltinTypeKind::Float => SemanticDataType::Float,
            BuiltinTypeKind::Double => SemanticDataType::Double,
            BuiltinTypeKind::String => SemanticDataType::String,
            BuiltinTypeKind::List => {
                let element_type = generic_types[0].clone();

                SemanticDataType::List(Box::new(element_type))
            }
            BuiltinTypeKind::Compound => {
                let element_type = generic_types[0].clone();

                SemanticDataType::Compound(Box::new(element_type))
            }
            BuiltinTypeKind::Data => {
                let element_type = generic_types[0].clone();

                let Some(data_type) = element_type.get_data_type(&ctx.semantic_environment) else {
                    let element_span = generic_spans[0];

                    return ctx.add_error_type(SemanticAnalysisError::TypeIsNotDataCompatible {
                        type_span: element_span,
                        data_type: element_type,
                    });
                };

                SemanticDataType::Data(Box::new(data_type))
            }
            BuiltinTypeKind::Score => {
                let element_type = generic_types[0].clone();

                if !element_type.is_score_compatible() {
                    let element_span = generic_spans[0];

                    return ctx.add_error_type(SemanticAnalysisError::TypeIsNotScoreCompatible {
                        type_span: element_span,
                        data_type: element_type,
                    });
                }

                SemanticDataType::Score(Box::new(element_type))
            }
            BuiltinTypeKind::EntitySelector => SemanticDataType::EntitySelector,
            BuiltinTypeKind::ResourceLocation => SemanticDataType::ResourceLocation,
            BuiltinTypeKind::Coordinates => SemanticDataType::Coordinates,
        }
    }
}
