use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighVisibleTypeId;
use crate::semantic::environment::value::HighVisibleValueId;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct SemanticTypedPathSegment {
    pub name: String,
    pub name_span: Span,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<SemanticDataType>,
}

impl SemanticTypedPathSegment {
    // #[must_use]
    // pub fn get_type_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleTypeId> {
    //     let Some(type_id) = self.type_id else {
    //         return ctx.add_error(SemanticAnalysisError::UnknownType {
    //             span: self.name_span,
    //             name: self.name.clone(),
    //         });
    //     };

    //     let declaration = ctx.semantic_environment.get_type(type_id);

    //     let expected_generic_count = declaration.kind.generic_count();
    //     let actual_generic_count = self.generic_types.len();

    //     if actual_generic_count != expected_generic_count {
    //         return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
    //             type_name_span: self.name_span,
    //             type_kind: declaration.kind.get_type_kind().into(),
    //             declaration_span: declaration.kind.name_span(),
    //             expected: expected_generic_count,
    //             actual: actual_generic_count,
    //         });
    //     }

    //     Some(type_id)
    // }

    // #[must_use]
    // pub fn get_value_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleValueId> {
    //     let Some(value_id) = self.value_id else {
    //         return ctx.add_error(SemanticAnalysisError::UnknownValue {
    //             span: self.name_span,
    //             name: self.name.clone(),
    //         });
    //     };

    //     Some(value_id)
    // }
}

#[derive(Debug, Clone)]
pub struct SemanticTypedPath {
    pub span: Span,
    pub segments: Vec<SemanticTypedPathSegment>,
}
