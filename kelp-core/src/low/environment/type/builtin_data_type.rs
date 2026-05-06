use crate::{
    high::{
        environment::r#type::builtin_data_type::BuiltinTypeKind,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::data_type::unresolved::UnresolvedDataType,
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinTypeId(pub u32);

#[derive(Debug, Clone)]
pub struct BuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

impl BuiltinTypeDeclaration {
    #[must_use]
    pub fn to_data_type_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
        mut generic_spans: Vec<Span>,
        mut generic_types: Vec<UnresolvedDataType>,
    ) -> UnresolvedDataType {
        let expected_generic_count = self.generic_count;
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_invalid_generics_type(
                span,
                &self.name,
                expected_generic_count,
                actual_generic_count,
            );
        }

        match self.kind {
            BuiltinTypeKind::Boolean => UnresolvedDataType::Boolean,
            BuiltinTypeKind::Byte => UnresolvedDataType::Byte,
            BuiltinTypeKind::Short => UnresolvedDataType::Short,
            BuiltinTypeKind::Integer => UnresolvedDataType::Integer,
            BuiltinTypeKind::Long => UnresolvedDataType::Long,
            BuiltinTypeKind::Float => UnresolvedDataType::Float,
            BuiltinTypeKind::Double => UnresolvedDataType::Double,
            BuiltinTypeKind::String => UnresolvedDataType::String,
            BuiltinTypeKind::List => {
                let element_type = generic_types.remove(0);

                UnresolvedDataType::List(Box::new(element_type))
            }
            BuiltinTypeKind::Compound => {
                let element_type = generic_types.remove(0);

                UnresolvedDataType::Compound(Box::new(element_type))
            }
            BuiltinTypeKind::Data => {
                let element_type = generic_types.remove(0);

                let Some(data_type) = element_type.get_data_type(&ctx.high_environment) else {
                    let element_span = generic_spans.remove(0);

                    return ctx.add_error_type(
                        element_span,
                        SemanticAnalysisError::TypeIsNotDataCompatible(element_type),
                    );
                };

                UnresolvedDataType::Data(Box::new(data_type))
            }
            BuiltinTypeKind::Score => {
                let element_type = generic_types.remove(0);

                if !element_type.is_score_compatible() {
                    let element_span = generic_spans.remove(0);

                    return ctx.add_error_type(
                        element_span,
                        SemanticAnalysisError::TypeIsNotDataCompatible(element_type),
                    );
                }

                UnresolvedDataType::Score(Box::new(element_type))
            }
            BuiltinTypeKind::EntitySelector => UnresolvedDataType::EntitySelector,
            BuiltinTypeKind::ResourceLocation => UnresolvedDataType::ResourceLocation,
            BuiltinTypeKind::Coordinates => UnresolvedDataType::Coordinates,
        }
    }
}
