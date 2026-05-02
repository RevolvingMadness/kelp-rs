use std::fmt::{Display, Write};

use crate::{
    high::{
        data_type::{resolved::GenericResolver, unresolved::UnresolvedDataType},
        semantic_analysis::SemanticAnalysisContext,
    },
    low::data_type::DataType,
    span::Span,
};

#[derive(Debug, Clone)]
pub struct GenericPathSegment<T> {
    pub name: String,
    pub name_span: Span,
    pub generic_types: Vec<T>,
}

impl<T: Display> Display for GenericPathSegment<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;

        if !self.generic_types.is_empty() {
            f.write_str("::<")?;

            for (i, data_type) in self.generic_types.iter().enumerate() {
                if i != 0 {
                    f.write_str(", ")?;
                }

                data_type.fmt(f)?;
            }

            f.write_char('>')?;
        }

        Ok(())
    }
}

impl GenericPathSegment<DataType> {
    #[must_use]
    pub fn resolve_fully(self, resolver: &GenericResolver) -> Option<Self> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_fully(resolver))
            .collect::<Option<_>>()?;

        Some(Self {
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
        context_generic_names: Option<&[String]>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<GenericPathSegment<DataType>> {
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

impl<T: Display> Display for GenericPath<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, segment) in self.segments.iter().enumerate() {
            if i != 0 {
                f.write_str("::")?;
            }

            segment.fmt(f)?;
        }

        Ok(())
    }
}

impl GenericPath<DataType> {
    #[must_use]
    pub fn resolve_fully(self, resolver: &GenericResolver) -> Option<Self> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_fully(resolver))
            .collect::<Option<_>>()?;

        Some(Self {
            span: self.span,
            segments,
        })
    }
}

impl GenericPath<UnresolvedDataType> {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<GenericPath<DataType>> {
        let partially_resolved = self.resolve_partially(None, ctx)?;

        partially_resolved.resolve_fully(&GenericResolver::empty())
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&[String]>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<GenericPath<DataType>> {
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

    #[must_use]
    pub fn single(span: Span, segment_name: &str) -> Self {
        Self {
            span,
            segments: vec![GenericPathSegment {
                name: segment_name.to_owned(),
                name_span: span,
                generic_types: Vec::new(),
            }],
        }
    }
}
