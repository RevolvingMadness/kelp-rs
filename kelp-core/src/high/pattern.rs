use std::collections::HashMap;

use crate::{
    high::{
        data::DataTarget,
        data_type::unresolved::UnresolvedDataType,
        nbt_path::NbtPath,
        player_score::PlayerScore,
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    low::expression::literal::LiteralExpression,
    middle::{data_type::DataType, pattern::Pattern as MiddlePattern},
    path::generic::GenericPath,
    pattern_type::PatternType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum PatternKind {
    Literal(LiteralExpression),

    Wildcard,
    Binding(GenericPath<UnresolvedDataType>),

    Score(PlayerScore),
    Data(DataTarget, NbtPath),

    Tuple(Vec<Pattern>),
    Struct(
        GenericPath<UnresolvedDataType>,
        HashMap<SNBTString, Pattern>,
    ),

    Compound(HashMap<SNBTString, Pattern>),
}

impl PatternKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Pattern {
        Pattern { span, kind: self }
    }

    #[must_use]
    pub fn is_irrefutable(&self) -> bool {
        match self {
            Self::Literal(_) => false,
            Self::Wildcard
            | Self::Binding(_)
            | Self::Compound(_)
            | Self::Score(_)
            | Self::Data(_, _) => true,
            Self::Tuple(patterns) => patterns.iter().all(|pattern| pattern.kind.is_irrefutable()),
            Self::Struct(_, field_patterns) => field_patterns
                .values()
                .all(|pattern| pattern.kind.is_irrefutable()),
        }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            Self::Literal(expression) => expression.get_pattern_type(),
            Self::Score(score) => PatternType::Score(score.clone()),
            Self::Data(target, path) => PatternType::Data(target.clone(), path.clone()),
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
            Self::Struct(path, field_patterns) => PatternType::Struct(
                path.clone(),
                field_patterns
                    .iter()
                    .map(|(key, pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(self, ctx: &mut SemanticAnalysisContext) {
        match self {
            Self::Literal(_) | Self::Score(_) | Self::Data(_, _) | Self::Wildcard => {}
            Self::Binding(path) => {
                if path.segments.len() != 1 {
                    return;
                }

                let name = path.segments[0].name.clone();

                let _ = ctx.declare_variable_unknown(name);
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
            Self::Struct(_, field_patterns) => {
                for pattern in field_patterns.into_values() {
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
    #[must_use]
    pub fn perform_irrefutablity_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        if !self.kind.is_irrefutable() {
            return ctx.add_error(self.span, SemanticAnalysisError::PatternIsNotIrrefutable);
        }

        Some(())
    }

    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        variable_type: DataType,
    ) -> Option<MiddlePattern> {
        let self_type = self.kind.get_type();

        let (wrappers, unwrapped_type) = variable_type.clone().unwrap_all();

        Some(match (self.kind, unwrapped_type) {
            (PatternKind::Literal(expression), _) => MiddlePattern::Literal(expression),
            (PatternKind::Wildcard, _) => MiddlePattern::Wildcard,
            (PatternKind::Binding(path), _) => {
                if path.segments.len() == 1 {
                    let name = path.segments[0].name.clone();

                    let id = ctx.declare_variable_known(name, variable_type);

                    MiddlePattern::Binding(id)
                } else {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::UnknownItem(path.to_string()),
                    );
                }
            }
            (PatternKind::Score(score), _) => {
                let score = score.perform_semantic_analysis(ctx)?;

                if !variable_type.is_score_compatible() {
                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type,
                            actual: self_type,
                        },
                    );
                }

                MiddlePattern::Score(score)
            }
            (PatternKind::Data(target, path), _) => {
                let target = target.perform_semantic_analysis(ctx)?;
                let path = path.perform_semantic_analysis(ctx)?;

                MiddlePattern::Data(target, path)
            }
            (PatternKind::Tuple(patterns), DataType::Tuple(data_types))
                if patterns.len() == data_types.len() =>
            {
                let patterns = patterns
                    .iter()
                    .cloned()
                    .zip(data_types)
                    .map(|(pattern, data_type)| {
                        let data_type = data_type.wrap_all(&wrappers);

                        pattern.perform_semantic_analysis(ctx, data_type)
                    })
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
                                    Some(data_type.clone())
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

                        let data_type = data_type.wrap_all(&wrappers);

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
                        let data_type = data_type.clone().wrap_all(&wrappers);

                        let pattern = pattern.clone().perform_semantic_analysis(ctx, data_type)?;

                        Some((key.snbt_string.clone(), pattern))
                    })
                    .collect_option_all()?;

                MiddlePattern::Compound(compound)
            }
            (PatternKind::Struct(path, field_patterns), DataType::Struct(value_id)) => {
                let mut path = path.resolve_fully(ctx)?;

                let pattern_id = ctx.resolve_type_generic_path(&path)?;

                let last_segment = path.segments.pop().unwrap();

                let pattern_declaration = ctx.get_type(pattern_id).clone();

                let pattern_type = pattern_declaration.resolve_fully(
                    ctx,
                    pattern_id,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?;

                let pattern_id = pattern_type.as_struct_id_semantic_analysis(
                    ctx,
                    last_segment.name_span,
                    &last_segment.name,
                )?;

                if pattern_id != value_id {
                    for pattern in field_patterns.into_values() {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type,
                            actual: self_type,
                        },
                    );
                }

                let pattern_declaration = ctx.get_struct_type(pattern_id);
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

                        let field_type = field_type.clone().wrap_all(&wrappers);

                        let pattern = pattern.perform_semantic_analysis(ctx, field_type)?;

                        Some((name.snbt_string.clone(), pattern))
                    })
                    .collect_option_all()?;

                MiddlePattern::Struct(pattern_id, field_patterns)
            }
            (kind, _) => {
                kind.destructure_unknown(ctx);

                return ctx.add_error(
                    self.span,
                    SemanticAnalysisError::MismatchedPatternTypes {
                        expected: variable_type,
                        actual: self_type,
                    },
                );
            }
        })
    }
}
