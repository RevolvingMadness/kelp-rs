use std::collections::HashMap;

use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        data::Data,
        data_type::DataType,
        environment::resolved::r#type::r#struct::{
            HighStructId, regular::HighRegularStructId, tuple::HighTupleStructId,
        },
        player_score::PlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    path::generic::GenericPath,
    pattern_type::PatternType,
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
    typed::{
        data_type::unresolved::UnresolvedDataType, expression::literal::LiteralExpression,
        pattern::TypedPattern,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum Pattern {
    Literal(LiteralExpression),

    Wildcard,
    Binding(GenericPath<DataType>),

    Score(PlayerScore),
    Data(Box<Data>),

    Tuple(Vec<Idx<Self>>),
    RegularStruct(GenericPath<DataType>, HashMap<(Span, String), Idx<Self>>),
    TupleStruct(GenericPath<DataType>, Vec<Idx<Self>>),

    Compound(HashMap<(Span, String), Idx<Self>>),
}

impl Pattern {
    #[must_use]
    pub fn get_type(id: Idx<Self>, allocator: &HighAstAllocator) -> PatternType {
        match allocator.get_pattern(id) {
            Self::Literal(expression) => expression.get_pattern_type(),
            Self::Score(score) => PatternType::Score(score.clone()),
            Self::Data(data) => PatternType::Data(data.clone()),
            Self::Wildcard | Self::Binding(..) => PatternType::Any,
            Self::Tuple(patterns) => PatternType::Tuple(
                patterns
                    .iter()
                    .map(|pattern| Self::get_type(*pattern, allocator))
                    .collect(),
            ),
            Self::Compound(compound) => PatternType::Compound(
                compound
                    .iter()
                    .map(|((_, key), pattern)| (key.clone(), Self::get_type(*pattern, allocator)))
                    .collect(),
            ),
            Self::RegularStruct(path, field_patterns) => PatternType::RegularStruct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|((_, key), pattern)| (key.clone(), Self::get_type(*pattern, allocator)))
                    .collect(),
            ),
            Self::TupleStruct(path, field_patterns) => PatternType::TupleStruct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|pattern| Self::get_type(*pattern, allocator))
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(
        id: Idx<Self>,
        allocator: &HighAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) {
        match allocator.get_pattern(id) {
            Self::Literal(..) | Self::Score(..) | Self::Data(..) | Self::Wildcard => {}

            Self::Binding(path) => {
                if path.segments.len() != 1 {
                    return;
                }

                let name = path.segments[0].name.clone();

                let _ = ctx.declare_variable(Visibility::Public, name, UnresolvedDataType::Error);
            }
            Self::Tuple(patterns) => {
                for pattern in patterns.iter().copied() {
                    Self::destructure_unknown(pattern, allocator, ctx);
                }
            }
            Self::Compound(compound) => {
                for pattern in compound.values().copied() {
                    Self::destructure_unknown(pattern, allocator, ctx);
                }
            }
            Self::RegularStruct(_, field_patterns) => {
                for pattern in field_patterns.values().copied() {
                    Self::destructure_unknown(pattern, allocator, ctx);
                }
            }
            Self::TupleStruct(_, field_patterns) => {
                for pattern in field_patterns.iter().copied() {
                    Self::destructure_unknown(pattern, allocator, ctx);
                }
            }
        }
    }

    pub fn perform_semantic_analysis(
        id: Idx<Self>,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
        variable_type: &UnresolvedDataType,
    ) -> Option<Idx<TypedPattern>> {
        let pattern = high_allocator.get_pattern(id);

        let self_type = Self::get_type(id, high_allocator);

        let (wrappers, inner_type) = variable_type.unwrap();

        Some(match (pattern, inner_type) {
            (Self::Literal(expression), _) => {
                low_allocator.allocate_pattern(TypedPattern::Literal(expression.clone()))
            }
            (Self::Wildcard, _) => low_allocator.allocate_pattern(TypedPattern::Wildcard),
            (Self::Binding(path), _) => {
                if path.segments.len() == 1 {
                    let segment = &path.segments[0];
                    let name = segment.name.clone();

                    let id = ctx.declare_variable(Visibility::Public, name, variable_type.clone());

                    low_allocator.allocate_pattern(TypedPattern::Binding(id))
                } else {
                    let span = high_allocator.get_pattern_span(id);

                    return ctx
                        .add_error(span, SemanticAnalysisError::UnknownItem(path.to_string()));
                }
            }
            (Self::Score(score), _) => {
                let score =
                    score
                        .clone()
                        .perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                if !variable_type.can_be_assigned_to_score() {
                    let span = high_allocator.get_pattern_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                low_allocator.allocate_pattern(TypedPattern::Score(score))
            }
            (Self::Data(data), _) => {
                let data =
                    data.clone()
                        .perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                if variable_type
                    .get_data_type(&ctx.resolved_environment)
                    .is_none()
                {
                    let span = high_allocator.get_pattern_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                low_allocator.allocate_pattern(TypedPattern::Data(data))
            }
            (Self::Tuple(patterns), UnresolvedDataType::Tuple(data_types))
                if patterns.len() == data_types.len() =>
            {
                let patterns = patterns
                    .iter()
                    .copied()
                    .zip(data_types)
                    .map(|(pattern, data_type)| {
                        Self::perform_semantic_analysis(
                            pattern,
                            high_allocator,
                            low_allocator,
                            ctx,
                            &data_type.clone().wrap(&wrappers),
                        )
                    })
                    .collect_option_all()?;

                low_allocator.allocate_pattern(TypedPattern::Tuple(patterns))
            }
            (
                Self::Compound(compound_patterns),
                UnresolvedDataType::TypedCompound(compound_types),
            ) => {
                let compound = compound_patterns
                    .iter()
                    .map(|((key_span, key), pattern)| {
                        let pattern = *pattern;

                        let Some(data_type) =
                            compound_types.iter().find_map(|(typed_key, data_type)| {
                                if typed_key == key {
                                    Some(data_type)
                                } else {
                                    None
                                }
                            })
                        else {
                            Self::destructure_unknown(pattern, high_allocator, ctx);

                            return ctx.add_error(
                                *key_span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: variable_type.clone(),
                                    field: key.clone(),
                                },
                            );
                        };

                        let pattern = Self::perform_semantic_analysis(
                            pattern,
                            high_allocator,
                            low_allocator,
                            ctx,
                            &data_type.clone().wrap(&wrappers),
                        )?;

                        Some((key.clone(), pattern))
                    })
                    .collect_option_all()?;

                low_allocator.allocate_pattern(TypedPattern::Compound(compound))
            }
            (Self::Compound(compound_patterns), UnresolvedDataType::Compound(data_type)) => {
                let compound = compound_patterns
                    .iter()
                    .map(|((_, key), pattern)| {
                        let pattern = *pattern;

                        let pattern = Self::perform_semantic_analysis(
                            pattern,
                            high_allocator,
                            low_allocator,
                            ctx,
                            &data_type.clone().wrap(&wrappers),
                        )?;

                        Some((key.clone(), pattern))
                    })
                    .collect_option_all()?;

                low_allocator.allocate_pattern(TypedPattern::Compound(compound))
            }
            (
                Self::RegularStruct(path, field_patterns),
                UnresolvedDataType::Struct(value_id, value_generic_types),
            ) => {
                let mut path = path.clone().perform_semantic_analysis(ctx);

                let pattern_id = ctx.get_visible_type_id(&path)?;
                let pattern_id = HighRegularStructId(pattern_id.0);

                let last_segment = path.segments.pop().unwrap();
                let pattern_generic_types = last_segment.generic_types;

                let (_, _, pattern_declaration) = ctx.get_visible_regular_struct(
                    last_segment.name_span,
                    &last_segment.name,
                    pattern_id.into(),
                )?;

                let pattern_generic_names = pattern_declaration.generic_ids.clone();

                if HighStructId::from(pattern_id) != *value_id
                    || pattern_generic_types != *value_generic_types
                {
                    for pattern in field_patterns.values().copied() {
                        Self::destructure_unknown(pattern, high_allocator, ctx);
                    }

                    let span = high_allocator.get_pattern_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                let field_types = pattern_declaration.field_types.clone();

                let field_patterns = field_patterns
                    .iter()
                    .map(|((name_span, name), pattern)| {
                        let pattern = *pattern;

                        let Some(field_type) = field_types.get(name) else {
                            Self::destructure_unknown(pattern, high_allocator, ctx);

                            return ctx.add_error(
                                *name_span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: variable_type.clone(),
                                    field: name.clone(),
                                },
                            );
                        };

                        let field_type = field_type
                            .clone()
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern = Self::perform_semantic_analysis(
                            pattern,
                            high_allocator,
                            low_allocator,
                            ctx,
                            &field_type.wrap(&wrappers),
                        )?;

                        Some((name.clone(), pattern))
                    })
                    .collect_option_all()?;

                low_allocator.allocate_pattern(TypedPattern::RegularStruct(
                    pattern_id,
                    pattern_generic_types,
                    field_patterns,
                ))
            }
            (
                Self::TupleStruct(path, field_patterns),
                UnresolvedDataType::Struct(value_id, value_generic_types),
            ) => {
                let mut path = path.clone().perform_semantic_analysis(ctx);

                let pattern_id = ctx.get_visible_type_id(&path)?;
                let pattern_id = HighTupleStructId(pattern_id.0);

                let last_segment = path.segments.pop().unwrap();
                let pattern_generic_types = last_segment.generic_types;

                let (_, _, pattern_declaration) = ctx.get_visible_tuple_struct(
                    last_segment.name_span,
                    &last_segment.name,
                    pattern_id.into(),
                )?;

                if HighStructId::from(pattern_id) != *value_id
                    || pattern_generic_types != *value_generic_types
                {
                    for pattern in field_patterns.iter().copied() {
                        Self::destructure_unknown(pattern, high_allocator, ctx);
                    }

                    let span = high_allocator.get_pattern_span(id);

                    return ctx.add_error(
                        span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                let field_types = pattern_declaration.field_types.clone();

                let expected_field_count = field_types.len();
                let actual_field_count = field_patterns.len();

                if expected_field_count != actual_field_count {
                    return ctx.add_error(
                        last_segment.name_span,
                        SemanticAnalysisError::MismatchedTupleStructFieldCount(
                            last_segment.name.clone(),
                            expected_field_count,
                            actual_field_count,
                        ),
                    );
                }

                let pattern_generic_names = pattern_declaration.generic_ids.clone();

                let field_patterns = field_patterns
                    .iter()
                    .copied()
                    .zip(field_types)
                    .map(|(field_pattern, field_type)| {
                        let field_type = field_type
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern = Self::perform_semantic_analysis(
                            field_pattern,
                            high_allocator,
                            low_allocator,
                            ctx,
                            &field_type.wrap(&wrappers),
                        )?;

                        Some(pattern)
                    })
                    .collect_option_all()?;

                low_allocator.allocate_pattern(TypedPattern::TupleStruct(
                    pattern_id,
                    pattern_generic_types,
                    field_patterns,
                ))
            }
            _ => {
                Self::destructure_unknown(id, high_allocator, ctx);

                let span = high_allocator.get_pattern_span(id);

                return ctx.add_error(
                    span,
                    SemanticAnalysisError::MismatchedPatternTypes {
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    },
                );
            }
        })
    }
}
