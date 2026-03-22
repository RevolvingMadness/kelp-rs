use crate::{
    high::{
        data_type::{
            resolved::{GenericResolver, PartiallyResolvedDataType},
            unresolved::UnresolvedDataType,
        },
        semantic_analysis_context::SemanticAnalysisContext,
    },
    middle::data_type::DataType,
    span::Span,
};

#[derive(Debug, Clone)]
pub struct GenericPathSegment<T> {
    pub name: String,
    pub name_span: Span,
    pub generic_types: Vec<T>,
}

impl GenericPathSegment<PartiallyResolvedDataType> {
    #[must_use]
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        resolver: &GenericResolver,
    ) -> Option<GenericPathSegment<DataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_fully(ctx, resolver))
            .collect::<Option<_>>()?;

        Some(GenericPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_types,
        })
    }
}

impl GenericPathSegment<UnresolvedDataType> {
    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<GenericPathSegment<PartiallyResolvedDataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_partially(context_generic_names, ctx))
            .collect::<Option<_>>()?;

        Some(GenericPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_types,
        })
    }
}

#[derive(Debug, Clone)]
pub struct GenericPath<T> {
    pub span: Span,
    pub segments: Vec<GenericPathSegment<T>>,
}

impl GenericPath<PartiallyResolvedDataType> {
    #[must_use]
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        resolver: &GenericResolver,
    ) -> Option<GenericPath<DataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_fully(ctx, resolver))
            .collect::<Option<_>>()?;

        Some(GenericPath {
            span: self.span,
            segments,
        })
    }
}

impl GenericPath<UnresolvedDataType> {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<GenericPath<DataType>> {
        let partially_resolved = self.resolve_partially(None, ctx)?;

        partially_resolved.resolve_fully(ctx, &GenericResolver::empty())
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<GenericPath<PartiallyResolvedDataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_partially(context_generic_names, ctx))
            .collect::<Option<_>>()?;

        Some(GenericPath {
            span: self.span,
            segments,
        })
    }
}
