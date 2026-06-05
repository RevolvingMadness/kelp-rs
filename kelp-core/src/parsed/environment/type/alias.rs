use crate::parsed::data_type::ParsedDataType;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::ItemKind;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
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
        let expected_generics = self.generic_ids.len();
        let actual_generics = generic_types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics_type(
                name_span,
                Some(self.name_span),
                expected_generics,
                actual_generics,
            );
        }

        let alias = self.alias.perform_semantic_analysis(ctx);

        alias.substitute_generics(&self.generic_ids, generic_types)
    }
}
