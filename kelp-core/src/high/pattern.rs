use std::collections::HashMap;

use crate::{
    high::{
        data::DataTarget,
        data_type::DataType,
        environment::r#type::r#struct::{
            HighStructId, regular::HighStructStructId, tuple::HighTupleStructId,
        },
        nbt_path::NbtPath,
        player_score::PlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::literal::LiteralExpression,
        pattern::UnresolvedPattern,
    },
    path::generic::GenericPath,
    pattern_type::PatternType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum PatternKind {
    Literal(LiteralExpression),

    Wildcard,
    Binding(GenericPath<DataType>),

    Score(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),

    Tuple(Vec<Pattern>),
    StructStruct(GenericPath<DataType>, HashMap<SNBTString, Pattern>),
    TupleStruct(GenericPath<DataType>, Vec<Pattern>),

    Compound(HashMap<SNBTString, Pattern>),
}

impl PatternKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Pattern {
        Pattern { span, kind: self }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            Self::Literal(expression) => expression.get_pattern_type(),
            Self::Score(score) => PatternType::Score(score.clone()),
            Self::Data(target_path) => {
                let (target, path) = &**target_path;

                PatternType::Data(Box::new((target.clone(), path.clone())))
            }
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
                    .map(|(key, pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
            Self::StructStruct(path, field_patterns) => PatternType::StructStruct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|(key, pattern)| (key.clone(), pattern.kind.get_type()))
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

                let name = path.segments[0].name.clone();

                let _ = ctx.declare_variable(Visibility::Public, name, UnresolvedDataType::Error);
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
            Self::StructStruct(_, field_patterns) => {
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
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

impl Pattern {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        variable_type: &UnresolvedDataType,
    ) -> Option<UnresolvedPattern> {
        let self_type = self.kind.get_type();

        Some(match (self.kind, variable_type) {
            (PatternKind::Literal(expression), _) => UnresolvedPattern::Literal(expression),
            (PatternKind::Wildcard, _) => UnresolvedPattern::Wildcard,
            (PatternKind::Binding(path), _) => {
                if path.segments.len() == 1 {
                    let segment = &path.segments[0];
                    let name = segment.name.clone();

                    let id = ctx.declare_variable(Visibility::Public, name, variable_type.clone());

                    UnresolvedPattern::Binding(id)
                } else {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::UnknownItem(path.to_string()),
                    );
                }
            }
            (PatternKind::Score(score), _) => {
                let score = score.perform_semantic_analysis(ctx)?;

                if !variable_type.can_be_assigned_to_score() {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                UnresolvedPattern::Score(score)
            }
            (PatternKind::Data(target_path), _) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx)?;
                let path = path.perform_semantic_analysis(ctx)?;

                if variable_type.get_data_type(&ctx.high_environment).is_none() {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                UnresolvedPattern::Data(Box::new((target, path)))
            }
            (PatternKind::Tuple(patterns), UnresolvedDataType::Tuple(data_types))
                if patterns.len() == data_types.len() =>
            {
                let patterns = patterns
                    .iter()
                    .cloned()
                    .zip(data_types)
                    .map(|(pattern, data_type)| pattern.perform_semantic_analysis(ctx, data_type))
                    .collect_option_all()?;

                UnresolvedPattern::Tuple(patterns)
            }
            (
                PatternKind::Compound(compound_patterns),
                UnresolvedDataType::TypedCompound(compound_types),
            ) => {
                let compound = compound_patterns
                    .into_iter()
                    .map(|(key, pattern)| {
                        let Some(data_type) =
                            compound_types.iter().find_map(|(typed_key, data_type)| {
                                if typed_key.1 == key.snbt_string.1 {
                                    Some(data_type)
                                } else {
                                    None
                                }
                            })
                        else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(
                                key.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: variable_type.clone(),
                                    field: key.snbt_string.1,
                                },
                            );
                        };

                        let pattern = pattern.perform_semantic_analysis(ctx, data_type)?;

                        Some((key.snbt_string, pattern))
                    })
                    .collect_option_all()?;

                UnresolvedPattern::Compound(compound)
            }
            (PatternKind::Compound(compound_patterns), UnresolvedDataType::Compound(data_type)) => {
                let compound = compound_patterns
                    .iter()
                    .map(|(key, pattern)| {
                        let pattern = pattern.clone().perform_semantic_analysis(ctx, data_type)?;

                        Some((key.snbt_string.clone(), pattern))
                    })
                    .collect_option_all()?;

                UnresolvedPattern::Compound(compound)
            }
            (
                PatternKind::StructStruct(path, field_patterns),
                UnresolvedDataType::Struct(value_id, value_generic_types),
            ) => {
                let mut path = path.resolve_partially(None, ctx);

                let pattern_id = ctx.get_visible_type_id(&path)?;
                let pattern_id = HighStructStructId(pattern_id.0);

                let last_segment = path.segments.pop().unwrap();
                let pattern_generic_types = last_segment.generic_types;

                let (_, _, pattern_declaration) = ctx.get_visible_struct_struct(
                    last_segment.name_span,
                    &last_segment.name,
                    pattern_id.into(),
                )?;

                let pattern_generic_names = pattern_declaration.generic_names.clone();

                if HighStructId::from(pattern_id) != *value_id
                    || pattern_generic_types != *value_generic_types
                {
                    for pattern in field_patterns.into_values() {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                let field_types = pattern_declaration.field_types.clone();

                let field_patterns = field_patterns
                    .into_iter()
                    .map(|(name, pattern)| {
                        let Some(field_type) = field_types.get(&name.snbt_string.1) else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(
                                name.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: UnresolvedDataType::Struct(
                                        pattern_id.into(),
                                        pattern_generic_types.clone(),
                                    ),
                                    field: name.snbt_string.1,
                                },
                            );
                        };

                        let field_type = field_type
                            .clone()
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern = pattern.perform_semantic_analysis(ctx, &field_type)?;

                        Some((name.snbt_string, pattern))
                    })
                    .collect_option_all()?;

                UnresolvedPattern::StructStruct(pattern_id, pattern_generic_types, field_patterns)
            }
            (
                PatternKind::TupleStruct(path, field_patterns),
                UnresolvedDataType::Struct(value_id, value_generic_types),
            ) => {
                let mut path = path.resolve_partially(None, ctx);

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
                    for pattern in field_patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(
                        self.span,
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

                let pattern_generic_names = pattern_declaration.generic_names.clone();

                let field_patterns = field_patterns
                    .into_iter()
                    .zip(field_types)
                    .map(|(field_pattern, field_type)| {
                        let field_type = field_type
                            .substitute_generics(&pattern_generic_names, &pattern_generic_types);

                        let pattern = field_pattern.perform_semantic_analysis(ctx, &field_type)?;

                        Some(pattern)
                    })
                    .collect_option_all()?;

                UnresolvedPattern::TupleStruct(pattern_id, pattern_generic_types, field_patterns)
            }
            (kind, _) => {
                kind.destructure_unknown(ctx);

                return ctx.add_error(
                    self.span,
                    SemanticAnalysisError::MismatchedPatternTypes {
                        expected: variable_type.clone(),
                        actual: Box::new(self_type),
                    },
                );
            }
        })
    }
}
