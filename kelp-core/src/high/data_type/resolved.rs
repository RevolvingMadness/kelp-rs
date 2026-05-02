use crate::{
    high::semantic_analysis::SemanticAnalysisContext, low::data_type::DataType, span::Span,
};

pub struct GenericResolver<'a> {
    names: &'a [String],
    types: &'a [DataType],
}

impl<'a> GenericResolver<'a> {
    pub fn create_semantic_analysis(
        ctx: &mut SemanticAnalysisContext,
        name: &str,
        name_span: Span,
        names: &'a [String],
        types: &'a [DataType],
    ) -> Option<Self> {
        let expected_generics = names.len();
        let actual_generics = types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics(
                name_span,
                name.to_owned(),
                expected_generics,
                actual_generics,
            );
        }

        Some(Self { names, types })
    }

    #[inline]
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            names: &[],
            types: &[],
        }
    }

    #[must_use]
    pub fn resolve(&self, name: &str) -> Option<&DataType> {
        let index = self.names.iter().position(|n| n == name)?;

        self.types.get(index)
    }
}
