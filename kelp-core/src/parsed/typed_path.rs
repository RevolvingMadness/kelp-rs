use std::fmt::{Display, Write};

use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighVisibleTypeId;
use crate::semantic::environment::value::HighVisibleValueId;
use crate::semantic::typed_path::SemanticTypedPathSegment;
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

pub struct ResolvedTypedPath {
    pub type_result: Result<HighVisibleTypeId, SemanticAnalysisError>,
    pub value_result: Result<HighVisibleValueId, SemanticAnalysisError>,
    pub inherited_generic_spans: Vec<Span>,
    pub inherited_generic_types: Vec<SemanticDataType>,
    pub generic_spans: Vec<Span>,
    pub generic_types: Vec<SemanticDataType>,
    pub name_span: Span,
    pub name: String,
}

impl ResolvedTypedPath {
    #[must_use]
    pub fn get_type_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleTypeId> {
        let type_id = match &self.type_result {
            Ok(id) => *id,
            Err(error) => return ctx.add_error(error.clone()),
        };

        let declaration = ctx.semantic_environment.get_type(type_id);

        let expected_generic_count = declaration.kind.generic_count();
        let actual_generic_count = self.generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                type_name_span: self.name_span,
                type_kind: declaration.kind.get_type_kind().into(),
                declaration_span: declaration.kind.name_span(),
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        Some(type_id)
    }

    #[must_use]
    pub fn get_value_id(&self, ctx: &mut SemanticAnalysisContext) -> Option<HighVisibleValueId> {
        let value_id = match &self.value_result {
            Ok(id) => *id,
            Err(error) => return ctx.add_error(error.clone()),
        };

        let declaration = ctx.semantic_environment.get_value(value_id);

        let expected_generic_count = declaration.kind.generic_count();
        let actual_generic_count = self.generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                type_name_span: self.name_span,
                type_kind: declaration.kind.get_value_kind().into(),
                declaration_span: declaration.kind.name_span(),
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        Some(value_id)
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
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ResolvedTypedPath> {
        assert!(!self.segments.is_empty());

        let mut segments = self
            .segments
            .into_iter()
            .map(|segment| segment.perform_semantic_analysis(ctx))
            .collect::<Vec<_>>();

        if segments.len() == 1 {
            let segment = segments.remove(0);

            let type_result = ctx
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get_type_id(&segment.name))
                .map(|id| HighVisibleTypeId(id.0))
                .ok_or_else(|| SemanticAnalysisError::UnknownType {
                    span: segment.name_span,
                    name: segment.name.clone(),
                });

            let value_result = ctx
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get_value_id(&segment.name))
                .map(|id| HighVisibleValueId(id.0))
                .ok_or_else(|| SemanticAnalysisError::UnknownValue {
                    span: segment.name_span,
                    name: segment.name.clone(),
                });

            return Some(ResolvedTypedPath {
                type_result,
                value_result,
                inherited_generic_spans: Vec::new(),
                inherited_generic_types: Vec::new(),
                generic_spans: segment.generic_spans,
                generic_types: segment.generic_types,
                name_span: segment.name_span,
                name: segment.name,
            });
        }

        let mut segments = segments.into_iter();

        let first_segment = segments.next().unwrap();
        let last_segment = segments.next_back().unwrap();

        let mut current_type_id = ctx
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_type_id(&first_segment.name))
            .map(|id| HighVisibleTypeId(id.0))
            .unwrap();

        let mut current_span = first_segment.name_span;

        let mut inherited_generic_spans = first_segment.generic_spans.clone();
        let mut inherited_generic_types = first_segment.generic_types;

        for segment in segments {
            let declaration = ctx.semantic_environment.get_type(current_type_id);

            let next_type_id = match declaration.get_visible_type_id(
                &ctx.semantic_environment,
                &ctx.current_module_path,
                current_type_id,
                current_span,
                &segment.name,
                segment.name_span,
            ) {
                Ok(id) => id,
                Err(error) => return ctx.add_error(error),
            };

            current_type_id = next_type_id;
            current_span = segment.name_span;

            inherited_generic_spans.extend(segment.generic_spans.iter().copied());
            inherited_generic_types.extend(segment.generic_types.iter().cloned());
        }

        let declaration = ctx.semantic_environment.get_type(current_type_id);

        let type_result = declaration.get_visible_type_id(
            &ctx.semantic_environment,
            &ctx.current_module_path,
            current_type_id,
            current_span,
            &last_segment.name,
            last_segment.name_span,
        );

        let value_result = declaration.get_visible_value_id(
            &ctx.semantic_environment,
            &ctx.current_module_path,
            current_type_id,
            current_span,
            &last_segment.name,
            last_segment.name_span,
        );

        Some(ResolvedTypedPath {
            type_result,
            value_result,
            inherited_generic_spans,
            inherited_generic_types,
            generic_spans: last_segment.generic_spans,
            generic_types: last_segment.generic_types,
            name_span: last_segment.name_span,
            name: last_segment.name,
        })
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
