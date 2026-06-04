use std::collections::HashMap;

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::{
    parsed::{
        data::Data,
        data_type::ParsedDataType,
        player_score::ParsedPlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    path::generic::TypedPath,
    pattern_type::PatternType,
    semantic::{expression::literal::SemanticLiteralExpression, pattern::SemanticPattern},
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum ParsedPatternKind {
    Literal(SemanticLiteralExpression),

    Wildcard,
    Binding(TypedPath<ParsedDataType>),

    Score(ParsedPlayerScore),
    Data(Box<Data>),

    Tuple(Vec<ParsedPattern>),
    RegularStruct(
        TypedPath<ParsedDataType>,
        HashMap<(Span, String), ParsedPattern>,
    ),
    TupleStruct(TypedPath<ParsedDataType>, Vec<ParsedPattern>),

    Compound(HashMap<(Span, String), ParsedPattern>),
}

impl ParsedPatternKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> ParsedPattern {
        ParsedPattern { span, kind: self }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            Self::Literal(expression) => expression.get_pattern_type(),
            Self::Score(score) => PatternType::Score(score.clone()),
            Self::Data(data) => PatternType::Data(data.clone()),
            Self::Wildcard | Self::Binding(..) => PatternType::Any,
            Self::Tuple(patterns) => PatternType::Tuple(
                patterns
                    .iter()
                    .map(|pattern| pattern.kind.get_type())
                    .collect(),
            ),
            Self::Compound(compound) => PatternType::Compound(
                compound
                    .iter()
                    .map(|((_, key), pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
            Self::RegularStruct(path, field_patterns) => PatternType::RegularStruct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|((_, key), pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
            Self::TupleStruct(path, field_patterns) => PatternType::TupleStruct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|pattern| pattern.kind.get_type())
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(self, ctx: &mut SemanticAnalysisContext) {
        match self {
            Self::Literal(..) | Self::Score(..) | Self::Data(..) | Self::Wildcard => {}

            Self::Binding(path) => {
                if path.segments.len() != 1 {
                    return;
                }

                let segment = &path.segments[0];
                let name_span = segment.name_span;
                let name = segment.name.clone();

                let _ = ctx.declare_variable(
                    Visibility::Public,
                    name_span,
                    name,
                    SemanticDataType::Error,
                );
            }
            Self::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::Compound(compound) => {
                for pattern in compound.into_values() {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::RegularStruct(_, field_patterns) => {
                for pattern in field_patterns.into_values() {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::TupleStruct(_, field_patterns) => {
                for pattern in field_patterns {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPattern {
    pub span: Span,
    pub kind: ParsedPatternKind,
}

impl ParsedPattern {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        variable_type: &SemanticDataType,
    ) -> Option<SemanticPattern> {
        let self_type = self.kind.get_type();

        let (wrappers, inner_type) = variable_type.unwrap();

        Some(match (self.kind, inner_type) {
            (ParsedPatternKind::Literal(expression), _) => SemanticPattern::Literal(expression),
            (ParsedPatternKind::Wildcard, _) => SemanticPattern::Wildcard,
            (ParsedPatternKind::Binding(path), _) => {
                if path.segments.len() == 1 {
                    let segment = &path.segments[0];
                    let name_span = segment.name_span;
                    let name = segment.name.clone();

                    let id = ctx.declare_variable(
                        Visibility::Public,
                        name_span,
                        name,
                        variable_type.clone(),
                    );

                    SemanticPattern::Binding(id)
                } else {
                    return ctx.add_error(SemanticAnalysisError::UnknownItem {
                        span: self.span,
                        name: path.to_string(),
                    });
                }
            }
            (ParsedPatternKind::Score(score), _) => {
                let score = score.perform_semantic_analysis(ctx)?;

                if !variable_type.can_be_assigned_to_score() {
                    return ctx.add_error(SemanticAnalysisError::MismatchedPatternTypes {
                        span: self.span,
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    });
                }

                SemanticPattern::Score(score)
            }
            (ParsedPatternKind::Data(data), _) => {
                let data = data.perform_semantic_analysis(ctx)?;

                if variable_type
                    .get_data_type(&ctx.semantic_environment)
                    .is_none()
                {
                    return ctx.add_error(SemanticAnalysisError::MismatchedPatternTypes {
                        span: self.span,
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    });
                }

                SemanticPattern::Data(data)
            }
            (ParsedPatternKind::Tuple(patterns), SemanticDataType::Tuple(data_types))
                if patterns.len() == data_types.len() =>
            {
                let patterns = patterns
                    .into_iter()
                    .zip(data_types)
                    .map(|(pattern, data_type)| {
                        pattern.perform_semantic_analysis(ctx, &data_type.clone().wrap(&wrappers))
                    })
                    .collect_option_all()?;

                SemanticPattern::Tuple(patterns)
            }
            (
                ParsedPatternKind::Compound(compound_patterns),
                SemanticDataType::TypedCompound(compound_types),
            ) => {
                let compound = compound_patterns
                    .into_iter()
                    .map(|((key_span, key), pattern)| {
                        let Some(data_type) =
                            compound_types.iter().find_map(|(typed_key, data_type)| {
                                if *typed_key == key {
                                    Some(data_type)
                                } else {
                                    None
                                }
                            })
                        else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(SemanticAnalysisError::TypeDoesntHaveField {
                                data_type: variable_type.clone(),
                                field_span: key_span,
                                field: key,
                            });
                        };

                        let pattern = pattern
                            .perform_semantic_analysis(ctx, &data_type.clone().wrap(&wrappers))?;

                        Some((key, pattern))
                    })
                    .collect_option_all()?;

                SemanticPattern::Compound(compound)
            }
            (
                ParsedPatternKind::Compound(compound_patterns),
                SemanticDataType::Compound(data_type),
            ) => {
                let compound = compound_patterns
                    .into_iter()
                    .map(|((_, key), pattern)| {
                        let pattern = pattern
                            .perform_semantic_analysis(ctx, &data_type.clone().wrap(&wrappers))?;

                        Some((key, pattern))
                    })
                    .collect_option_all()?;

                SemanticPattern::Compound(compound)
            }
            (
                ParsedPatternKind::RegularStruct(path, field_patterns),
                SemanticDataType::Struct(value_id, value_generic_types),
            ) => {
                let path = path.perform_semantic_analysis(ctx);

                let (pattern_id, _, pattern_generic_types, last_segment) =
                    ctx.get_visible_type_id(path)?;

                let (pattern_id, pattern_type) = ctx.get_struct_id(pattern_id, &last_segment)?;

                let (pattern_id, pattern_declaration) =
                    ctx.get_regular_struct(pattern_id, pattern_type, last_segment.name_span)?;

                let pattern_generic_names = pattern_declaration.generic_ids.clone();

                if HighStructId::from(pattern_id) != *value_id
                    || pattern_generic_types != *value_generic_types
                {
                    for pattern in field_patterns.into_values() {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(SemanticAnalysisError::MismatchedPatternTypes {
                        span: self.span,
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    });
                }

                let field_types = pattern_declaration.field_types.clone();

                let field_patterns = field_patterns
                    .into_iter()
                    .map(|((name_span, name), pattern)| {
                        let Some(field_type) = field_types.get(&name) else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(SemanticAnalysisError::TypeDoesntHaveField {
                                field_span: name_span,
                                data_type: variable_type.clone(),
                                field: name,
                            });
                        };

                        let field_type = field_type
                            .clone()
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern =
                            pattern.perform_semantic_analysis(ctx, &field_type.wrap(&wrappers))?;

                        Some((name, pattern))
                    })
                    .collect_option_all()?;

                SemanticPattern::RegularStruct(pattern_id, pattern_generic_types, field_patterns)
            }
            (
                ParsedPatternKind::TupleStruct(path, field_patterns),
                SemanticDataType::Struct(value_id, value_generic_types),
            ) => {
                let path = path.perform_semantic_analysis(ctx);

                let (pattern_id, _, pattern_generic_types, last_segment) =
                    ctx.get_visible_type_id(path)?;

                let (pattern_id, pattern_type) = ctx.get_struct_id(pattern_id, &last_segment)?;

                let (pattern_id, pattern_declaration) =
                    ctx.get_tuple_struct(pattern_id, pattern_type, last_segment.name_span)?;

                if HighStructId::from(pattern_id) != *value_id
                    || pattern_generic_types != *value_generic_types
                {
                    for pattern in field_patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(SemanticAnalysisError::MismatchedPatternTypes {
                        span: self.span,
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    });
                }

                let field_types = pattern_declaration.field_types.clone();

                let expected_field_count = field_types.len();
                let actual_field_count = field_patterns.len();

                if expected_field_count != actual_field_count {
                    return ctx.add_error(SemanticAnalysisError::MismatchedTupleStructFieldCount {
                        name_span: last_segment.name_span,
                        name: last_segment.name.clone(),
                        expected: expected_field_count,
                        actual: actual_field_count,
                    });
                }

                let pattern_generic_names = pattern_declaration.generic_ids.clone();

                let field_patterns = field_patterns
                    .into_iter()
                    .zip(field_types)
                    .map(|(field_pattern, field_type)| {
                        let field_type = field_type
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern = field_pattern
                            .perform_semantic_analysis(ctx, &field_type.wrap(&wrappers))?;

                        Some(pattern)
                    })
                    .collect_option_all()?;

                SemanticPattern::TupleStruct(pattern_id, pattern_generic_types, field_patterns)
            }
            (kind, _) => {
                kind.destructure_unknown(ctx);

                return ctx.add_error(SemanticAnalysisError::MismatchedPatternTypes {
                    span: self.span,
                    expected: variable_type.clone(),
                    actual: Box::new(self_type),
                });
            }
        })
    }
}
