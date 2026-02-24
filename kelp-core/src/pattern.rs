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
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> Pattern {
        Pattern { span, kind: self }
    }

    #[must_use]
    pub fn is_irrefutable(&self) -> bool {
        match self {
            PatternKind::Literal(_) => false,
            PatternKind::Wildcard
            | PatternKind::Binding(_)
            | PatternKind::Compound(_)
            | PatternKind::Dereference(_)
            | PatternKind::Struct(_, _) => true,
            PatternKind::Tuple(patterns) => {
                patterns.iter().all(|pattern| pattern.kind.is_irrefutable())
            }
        }
    }

    #[must_use]
    pub fn get_type(&self) -> PatternType {
        match self {
            PatternKind::Literal(expression) => expression.kind.get_pattern_type(),
            PatternKind::Wildcard | PatternKind::Binding(_) => PatternType::Any,
            PatternKind::Tuple(patterns) => PatternType::Tuple(
                patterns
                    .iter()
                    .map(|pattern| pattern.kind.get_type())
                    .collect(),
            ),
            PatternKind::Compound(compound) => PatternType::Compound(
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
            PatternKind::Dereference(pattern) => {
                PatternType::Dereference(Box::new(pattern.kind.get_type()))
            }
            PatternKind::Struct(name, field_patterns) => PatternType::Struct(
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
            PatternKind::Literal(_) | PatternKind::Wildcard => {}
            PatternKind::Binding(name) => {
                ctx.declare_variable_unknown(name);
            }
            PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
            PatternKind::Compound(compound) => {
                for (key, pattern) in compound {
                    if let Some(pattern) = pattern {
                        pattern.kind.destructure_unknown(ctx);
                    } else {
                        ctx.declare_variable_unknown(&key.snbt_string.1);
                    }
                }
            }
            PatternKind::Dereference(pattern) => {
                pattern.kind.destructure_unknown(ctx);
            }
            PatternKind::Struct(_, field_patterns) => {
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
