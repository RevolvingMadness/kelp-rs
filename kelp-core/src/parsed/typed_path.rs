use std::fmt::{Display, Write};

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::typed_path::{SemanticTypedPath, SemanticTypedPathSegment};
use crate::{
    parsed::{data_type::ParsedDataType, semantic_analysis::SemanticAnalysisContext},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct ParsedTypedPathSegment {
    pub name: String,
    pub name_span: Span,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<ParsedDataType>,
}

impl Display for ParsedTypedPathSegment {
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

impl ParsedTypedPathSegment {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> SemanticTypedPathSegment {
        let generic_types = self
            .generic_types
            .into_iter()
            .map(|data_type| data_type.perform_semantic_analysis(ctx))
            .collect();

        SemanticTypedPathSegment {
            name: self.name,
            name_span: self.name_span,
            generic_spans: self.generic_spans,
            generic_types,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypedPath {
    pub span: Span,
    pub segments: Vec<ParsedTypedPathSegment>,
}

impl Display for ParsedTypedPath {
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

impl ParsedTypedPath {
    #[must_use]
    pub fn perform_semantic_analysis(self, ctx: &mut SemanticAnalysisContext) -> SemanticTypedPath {
        let segments = self
            .segments
            .into_iter()
            .map(|segment| segment.perform_semantic_analysis(ctx))
            .collect();

        SemanticTypedPath {
            span: self.span,
            segments,
        }
    }

    #[must_use]
    pub fn single(span: Span, segment_name: &str) -> Self {
        Self {
            span,
            segments: vec![ParsedTypedPathSegment {
                name: segment_name.to_owned(),
                name_span: span,
                generic_spans: Vec::new(),
                generic_types: Vec::new(),
            }],
        }
    }
}
