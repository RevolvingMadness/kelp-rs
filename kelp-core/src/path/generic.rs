use std::fmt::{Display, Write};

use crate::low::data_type::DataType;
use crate::semantic::data_type::SemanticDataType;
use crate::{
    datapack::Datapack,
    parsed::{data_type::ParsedDataType, semantic_analysis::SemanticAnalysisContext},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct TypedPathSegment<T> {
    pub name: String,
    pub name_span: Span,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<T>,
}

impl Display for TypedPathSegment<ParsedDataType> {
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

impl TypedPathSegment<SemanticDataType> {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> Option<TypedPathSegment<DataType>> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.resolve(datapack))
            .collect::<Option<_>>()?;

        Some(TypedPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_spans: self.generic_spans,
            generic_types,
        })
    }
}

impl TypedPathSegment<ParsedDataType> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> TypedPathSegment<SemanticDataType> {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.perform_semantic_analysis(ctx))
            .collect();

        TypedPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_spans: self.generic_spans,
            generic_types,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedPath<T> {
    pub span: Span,
    pub segments: Vec<TypedPathSegment<T>>,
}

impl Display for TypedPath<ParsedDataType> {
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

impl TypedPath<SemanticDataType> {
    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> Option<TypedPath<DataType>> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.resolve(datapack))
            .collect::<Option<_>>()?;

        Some(TypedPath {
            span: self.span,
            segments,
        })
    }
}

impl TypedPath<ParsedDataType> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> TypedPath<SemanticDataType> {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.perform_semantic_analysis(ctx))
            .collect();

        TypedPath {
            span: self.span,
            segments,
        }
    }

    #[must_use]
    pub fn single(span: Span, segment_name: &str) -> Self {
        Self {
            span,
            segments: vec![TypedPathSegment {
                name: segment_name.to_owned(),
                name_span: span,
                generic_spans: Vec::new(),
                generic_types: Vec::new(),
            }],
        }
    }
}
