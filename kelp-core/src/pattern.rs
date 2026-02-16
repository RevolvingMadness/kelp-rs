use std::collections::BTreeMap;

use parser_rs::parser_range::ParserRange;

use crate::{
    expression::literal::LiteralExpression,
    high::snbt_string::HighSNBTString,
    pattern_type::PatternType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
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
    pub fn is_irrefutable(&self) -> bool {
        match self {
            PatternKind::Literal(_) => false,
            PatternKind::Wildcard => true,
            PatternKind::Binding(_) => true,
            PatternKind::Tuple(patterns) => {
                patterns.iter().all(|pattern| pattern.kind.is_irrefutable())
            }
            PatternKind::Compound(_) => true,
            PatternKind::Dereference(_) => true,
            PatternKind::Struct(_, _) => true,
        }
    }

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
                                .map(|pattern| pattern.kind.get_type())
                                .unwrap_or(PatternType::Any),
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
                                .map(|pattern| pattern.kind.get_type())
                                .unwrap_or(PatternType::Any),
                        )
                    })
                    .collect(),
            ),
        }
    }

    pub fn destructure_unknown(&self, ctx: &mut SemanticAnalysisContext) {
        match self {
            PatternKind::Literal(_) => {}
            PatternKind::Wildcard => {}
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
    pub span: ParserRange,
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
