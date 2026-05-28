use crate::parsed::data_type::ParsedDataType;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::path::generic::GenericPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ParsedTypeAliasDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: ParsedDataType,
}

impl ParsedTypeAliasDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        let expected_generics = self.generic_ids.len();
        let actual_generics = segment.generic_types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics_type(
                segment.name_span,
                self.name(),
                expected_generics,
                actual_generics,
            );
        }

        let alias = self.alias.perform_semantic_analysis(ctx);

        alias.substitute_generics(&self.generic_ids, &segment.generic_types)
    }
}
