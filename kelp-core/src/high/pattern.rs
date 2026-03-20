use std::collections::HashMap;

use crate::{
    high::{
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    low::expression::literal::LiteralExpression,
    middle::{data_type::DataType, pattern::Pattern as MiddlePattern},
    pattern_type::PatternType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Literal(LiteralExpression),

    Wildcard,
    Binding(String),

    Tuple(Vec<Pattern>),
    Struct(String, HashMap<SNBTString, Pattern>),

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
            Self::Wildcard | Self::Binding(_) | Self::Compound(_) | Self::Struct(_, _) => true,
            Self::Tuple(patterns) => patterns.iter().all(|pattern| pattern.kind.is_irrefutable()),
        }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            Self::Literal(expression) => expression.get_pattern_type(),
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
            Self::Struct(name, field_patterns) => PatternType::Struct(
                name.clone(),
                field_patterns
                    .iter()
                    .map(|(key, pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(&self, ctx: &mut SemanticAnalysisContext) {
        match self {
            Self::Literal(_) | Self::Wildcard => {}
            Self::Binding(name) => {
                let _ = ctx.declare_variable_unknown(name.clone());
            }
            Self::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::Compound(compound) => {
                for pattern in compound.values() {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::Struct(_, field_patterns) => {
                for pattern in field_patterns.values() {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
        let (wrappers, unwrapped_type) = variable_type.clone().unwrap_all();

        Some(match self.kind {
            PatternKind::Literal(expression) => MiddlePattern::Literal(expression),
            PatternKind::Wildcard => MiddlePattern::Wildcard,
            PatternKind::Binding(name) => {
                let id = ctx.declare_variable_known(name, variable_type);

                MiddlePattern::Binding(id)
            }
            PatternKind::Tuple(ref patterns) => match unwrapped_type {
                DataType::Tuple(data_types) if patterns.len() == data_types.len() => {
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
                _ => {
                    for pattern in patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type,
                            actual: self.kind.get_type(),
                        },
                    );
                }
            },
            PatternKind::Compound(ref compound) => match unwrapped_type {
                DataType::TypedCompound(ref data_types) => {
                    let compound = compound
                        .iter()
                        .map(|(key, pattern)| {
                            let Some(data_type) =
                                data_types.iter().find_map(|(typed_key, data_type)| {
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
                                        field: key.snbt_string.1.clone(),
                                    },
                                );
                            };

                            let data_type = data_type.wrap_all(&wrappers);

                            let pattern =
                                pattern.clone().perform_semantic_analysis(ctx, data_type)?;

                            Some((key.snbt_string.clone(), pattern))
                        })
                        .collect_option_all()?;

                    MiddlePattern::Compound(compound)
                }
                DataType::Compound(data_type) => {
                    let compound = compound
                        .iter()
                        .map(|(key, pattern)| {
                            let data_type = data_type.clone().wrap_all(&wrappers);

                            let pattern =
                                pattern.clone().perform_semantic_analysis(ctx, data_type)?;

                            Some((key.snbt_string.clone(), pattern))
                        })
                        .collect_option_all()?;

                    MiddlePattern::Compound(compound)
                }
                _ => {
                    self.kind.destructure_unknown(ctx);

                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type,
                            actual: self.kind.get_type(),
                        },
                    );
                }
            },
            PatternKind::Struct(_, ref field_patterns) => {
                let DataType::Struct(id) = unwrapped_type else {
                    self.kind.destructure_unknown(ctx);

                    return ctx.add_error(
                        self.span,
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: variable_type,
                            actual: self.kind.get_type(),
                        },
                    );
                };

                let declaration = ctx.get_struct_type(id);
                let field_types = declaration.field_types.clone();

                let field_patterns = field_patterns
                    .iter()
                    .map(|(name, pattern)| {
                        let Some(field_type) = field_types.get(&name.snbt_string.1) else {
                            pattern.kind.destructure_unknown(ctx);

                            return ctx.add_error(
                                name.span,
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: DataType::Struct(id),
                                    field: name.snbt_string.1.clone(),
                                },
                            );
                        };

                        let field_type = field_type.clone().wrap_all(&wrappers);

                        let pattern = pattern.clone().perform_semantic_analysis(ctx, field_type)?;

                        Some((name.snbt_string.clone(), pattern))
                    })
                    .collect_option_all()?;

                MiddlePattern::Struct(id, field_patterns)
            }
        })
    }
}
