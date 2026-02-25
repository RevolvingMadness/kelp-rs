use std::collections::BTreeMap;

use crate::{
    expression::literal::LiteralExpression,
    high::snbt_string::HighSNBTString,
    pattern_type::PatternType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Literal(LiteralExpression),

    Wildcard,
    Binding(String),

    Tuple(Vec<Pattern>),
    Struct(String, BTreeMap<HighSNBTString, Option<Pattern>>),

    Compound(BTreeMap<HighSNBTString, Option<Pattern>>),

    Dereference(Box<Pattern>),
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
            | Self::Dereference(_)
            | Self::Struct(_, _) => true,
            Self::Tuple(patterns) => patterns.iter().all(|pattern| pattern.kind.is_irrefutable()),
        }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            Self::Literal(expression) => expression.kind.get_pattern_type(),
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
                    .map(|(key, pattern)| {
                        (
                            key.clone(),
                            pattern
                                .as_ref()
                                .map_or(PatternType::Any, |pattern| pattern.kind.get_type()),
                        )
                    })
                    .collect(),
            ),
            Self::Dereference(pattern) => {
                PatternType::Dereference(Box::new(pattern.kind.get_type()))
            }
            Self::Struct(name, field_patterns) => PatternType::Struct(
                name.clone(),
                field_patterns
                    .iter()
                    .map(|(key, pattern)| {
                        (
                            key.clone(),
                            pattern
                                .as_ref()
                                .map_or(PatternType::Any, |pattern| pattern.kind.get_type()),
                        )
                    })
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(&self, ctx: &mut SemanticAnalysisContext) {
        match self {
            Self::Literal(_) | Self::Wildcard => {}
            Self::Binding(name) => {
                ctx.declare_variable_unknown(name);
            }
            Self::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            Self::Compound(compound) => {
                for (key, pattern) in compound {
                    if let Some(pattern) = pattern {
                        pattern.kind.destructure_unknown(ctx);
                    } else {
                        ctx.declare_variable_unknown(&key.snbt_string.1);
                    }
                }
            }
            Self::Dereference(pattern) => {
                pattern.kind.destructure_unknown(ctx);
            }
            Self::Struct(_, field_patterns) => {
                for (key, pattern) in field_patterns {
                    if let Some(pattern) = pattern {
                        pattern.kind.destructure_unknown(ctx);
                    } else {
                        ctx.declare_variable_unknown(&key.snbt_string.1);
                    }
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
            return ctx.add_info(SemanticAnalysisInfo {
                span: self.span,
                kind: SemanticAnalysisInfoKind::Error(
                    SemanticAnalysisError::PatternIsNotIrrefutable,
                ),
            });
        }

        Some(())
    }
}
