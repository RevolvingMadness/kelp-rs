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
pub struct PathSegment<T> {
    pub name: String,
    pub span: Span,
    pub generic_types: Vec<T>,
}

impl PathSegment<PartiallyResolvedDataType> {
    #[must_use]
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        resolver: &GenericResolver,
    ) -> Option<PathSegment<DataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_fully(ctx, resolver))
            .collect::<Option<_>>()?;

        Some(PathSegment {
            name: self.name,
            span: self.span,
            generic_types,
        })
    }
}

impl PathSegment<UnresolvedDataType> {
    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<PathSegment<PartiallyResolvedDataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_partially(context_generic_names, ctx))
            .collect::<Option<_>>()?;

        Some(PathSegment {
            name: self.name,
            span: self.span,
            generic_types,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Path<T> {
    pub span: Span,
    pub segments: Vec<PathSegment<T>>,
}

impl Path<PartiallyResolvedDataType> {
    #[must_use]
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        resolver: &GenericResolver,
    ) -> Option<Path<DataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_fully(ctx, resolver))
            .collect::<Option<_>>()?;

        Some(Path {
            span: self.span,
            segments,
        })
    }
}

impl Path<UnresolvedDataType> {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<Path<DataType>> {
        let partially_resolved = self.resolve_partially(None, ctx)?;

        partially_resolved.resolve_fully(ctx, &GenericResolver::empty())
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Path<PartiallyResolvedDataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_partially(context_generic_names, ctx))
            .collect::<Option<_>>()?;

        Some(Path {
            span: self.span,
            segments,
        })
    }
}
