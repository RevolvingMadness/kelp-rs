use crate::parsed::data_type::ParsedDataType;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, TypeKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::generic::HighGenericId;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedTypeAliasDeclaration {
    pub name_span: Span,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: ParsedDataType,
}

impl ParsedTypeAliasDeclaration {
    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        name_span: Span,
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        let expected_generic_count = self.generic_ids.len();
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                type_name_span: name_span,
                type_kind: TypeKind::Alias.into(),
                declaration_span: Some(self.name_span),
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        let alias = self.alias.perform_semantic_analysis(ctx);

        alias.substitute_generics(&self.generic_ids, generic_types)
    }
}
