use std::collections::HashMap;

use crate::{
    high::{
        data::DataTarget,
        data_type::UnresolvedDataType,
        nbt_path::NbtPath,
        player_score::PlayerScore,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    low::{
        data_type::DataType, environment::value::variable::VariableId,
        expression::literal::LiteralExpression, pattern::Pattern as MiddlePattern,
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
    Binding(GenericPath<UnresolvedDataType>),

    Score(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),

    Tuple(Vec<Pattern>),
    StructStruct(
        GenericPath<UnresolvedDataType>,
        HashMap<SNBTString, Pattern>,
    ),
    TupleStruct(GenericPath<UnresolvedDataType>, Vec<Pattern>),

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
            Self::Wildcard | Self::Binding(_) => PatternType::Any,
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
            Self::Literal(_) | Self::Score(_) | Self::Data(_) | Self::Wildcard => {}
            Self::Binding(path) => {
                if path.segments.len() != 1 {
                    return;
                }

                let name = path.segments[0].name.clone();

                let _ = ctx.declare_variable_unknown(Visibility::Public, name);
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
        variable_type: &DataType,
    ) -> Option<MiddlePattern> {
        let self_type = self.kind.get_type();

        Some(match (self.kind, variable_type) {
            (PatternKind::Literal(expression), _) => MiddlePattern::Literal(expression),
            (PatternKind::Wildcard, _) => MiddlePattern::Wildcard,
            (PatternKind::Binding(path), _) => {
                if path.segments.len() == 1 {
                    let segment = &path.segments[0];
                    let name = segment.name.clone();

                    let id =
                        ctx.declare_variable_known(Visibility::Public, name, variable_type.clone());

                    let declaration = ctx.get_value(id.into()).clone();

                    let (resolved_id, _) =
                        declaration.resolve_fully(ctx, id.into(), Vec::new(), segment.name_span)?;

                    MiddlePattern::Binding(VariableId(resolved_id.0))
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

                MiddlePattern::Score(score)
            }
            (PatternKind::Data(target_path), _) => {
                let (target, path) = *target_path;

                let target = target.perform_semantic_analysis(ctx)?;
                let path = path.perform_semantic_analysis(ctx)?;

                if variable_type.get_data_type(&ctx.environment).is_none() {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type.clone(),
                            actual: Box::new(self_type),
                        },
                    );
                }

                MiddlePattern::Data(Box::new((target, path)))
            }
            (PatternKind::Tuple(patterns), DataType::Tuple(data_types))
                if patterns.len() == data_types.len() =>
            {
                let patterns = patterns
                    .iter()
                    .cloned()
                    .zip(data_types)
                    .map(|(pattern, data_type)| pattern.perform_semantic_analysis(ctx, data_type))
                    .collect_option_all()?;

                MiddlePattern::Tuple(patterns)
            }
            (PatternKind::Compound(compound_patterns), DataType::TypedCompound(compound_types)) => {
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

                MiddlePattern::Compound(compound)
            }
            (PatternKind::Compound(compound_patterns), DataType::Compound(data_type)) => {
                let compound = compound_patterns
                    .iter()
                    .map(|(key, pattern)| {
                        let pattern = pattern.clone().perform_semantic_analysis(ctx, data_type)?;

                        Some((key.snbt_string.clone(), pattern))
                    })
                    .collect_option_all()?;

                MiddlePattern::Compound(compound)
            }
            (PatternKind::StructStruct(path, field_patterns), DataType::Struct(value_id)) => {
                let mut path = path.resolve_fully(ctx)?;

                let pattern_id = ctx.get_visible_type_id(&path)?;

                let last_segment = path.segments.pop().unwrap();

                let pattern_declaration = ctx.get_type(pattern_id).clone();

                let pattern_type = pattern_declaration.resolve_fully(
                    ctx,
                    pattern_id,
                    last_segment.generic_spans,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?;

                let pattern_id = pattern_type.as_struct_id_semantic_analysis(
                    ctx,
                    last_segment.name_span,
                    &last_segment.name,
                )?;

                if pattern_id != *value_id {
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

                let (specific_id, _, pattern_declaration) = ctx.get_visible_struct_struct(
                    last_segment.name_span,
                    &last_segment.name,
                    pattern_id,
                )?;

                let field_types = pattern_declaration.field_types.clone();

                let field_patterns = field_patterns
                    .into_iter()
                    .map(|(name, pattern)| {
                        let Some(field_type) = field_types.get(&name.snbt_string.1) else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(
                                name.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: DataType::Struct(pattern_id),
                                    field: name.snbt_string.1,
                                },
                            );
                        };

                        let pattern = pattern.perform_semantic_analysis(ctx, field_type)?;

                        Some((name.snbt_string, pattern))
                    })
                    .collect_option_all()?;

                MiddlePattern::StructStruct(specific_id, field_patterns)
            }
            (PatternKind::TupleStruct(path, field_patterns), DataType::Struct(value_id)) => {
                let mut path = path.resolve_fully(ctx)?;

                let pattern_id = ctx.get_visible_type_id(&path)?;

                let last_segment = path.segments.pop().unwrap();

                let pattern_declaration = ctx.get_type(pattern_id).clone();

                let pattern_type = pattern_declaration.resolve_fully(
                    ctx,
                    pattern_id,
                    last_segment.generic_spans,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?;

                let pattern_id = pattern_type.as_struct_id_semantic_analysis(
                    ctx,
                    last_segment.name_span,
                    &last_segment.name,
                )?;

                if pattern_id != *value_id {
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

                let (specific_id, _, pattern_declaration) = ctx.get_visible_tuple_struct(
                    last_segment.name_span,
                    &last_segment.name,
                    pattern_id,
                )?;

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

                let field_patterns = field_patterns
                    .into_iter()
                    .zip(field_types)
                    .map(|(field_pattern, field_type)| {
                        let pattern = field_pattern.perform_semantic_analysis(ctx, &field_type)?;

                        Some(pattern)
                    })
                    .collect_option_all()?;

                MiddlePattern::TupleStruct(specific_id, field_patterns)
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
