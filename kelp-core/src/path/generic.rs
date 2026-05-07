use std::fmt::{Display, Write};

use crate::{
    datapack::Datapack,
    high::{data_type::DataType, semantic_analysis::SemanticAnalysisContext},
    low::data_type::{resolved::ResolvedDataType, unresolved::UnresolvedDataType},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct GenericPathSegment<T> {
    pub name: String,
    pub name_span: Span,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<T>,
}

impl Display for GenericPathSegment<DataType> {
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

impl GenericPathSegment<UnresolvedDataType> {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> Option<GenericPathSegment<ResolvedDataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve(datapack))
            .collect::<Option<_>>()?;

        Some(GenericPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_spans: self.generic_spans,
            generic_types,
        })
    }
}

impl GenericPathSegment<DataType> {
    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&[String]>,
        ctx: &mut SemanticAnalysisContext,
    ) -> GenericPathSegment<UnresolvedDataType> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve_partially(context_generic_names, ctx))
            .collect();

        GenericPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_spans: self.generic_spans,
            generic_types,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericPath<T> {
    pub span: Span,
    pub segments: Vec<GenericPathSegment<T>>,
}

impl Display for GenericPath<DataType> {
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

impl GenericPath<UnresolvedDataType> {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> Option<GenericPath<ResolvedDataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve(datapack))
            .collect::<Option<_>>()?;

        Some(GenericPath {
            span: self.span,
            segments,
        })
    }
}

impl GenericPath<DataType> {
    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&[String]>,
        ctx: &mut SemanticAnalysisContext,
    ) -> GenericPath<UnresolvedDataType> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve_partially(context_generic_names, ctx))
            .collect();

        GenericPath {
            span: self.span,
            segments,
        }
    }

    #[must_use]
    pub fn single(span: Span, segment_name: &str) -> Self {
        Self {
            span,
            segments: vec![GenericPathSegment {
                name: segment_name.to_owned(),
                name_span: span,
                generic_spans: Vec::new(),
                generic_types: Vec::new(),
            }],
        }
    }
}
