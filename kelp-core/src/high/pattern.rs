use std::collections::HashMap;

use crate::{
    high::snbt_string::SNBTString,
    low::expression::literal::LiteralExpression,
    middle::{data_type::DataType, pattern::Pattern as MiddlePattern},
    pattern_type::PatternType,
    semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
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
                ctx.declare_variable_unknown(name);
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
        let variable_type = variable_type.distribute();

        Some(match self.kind {
            PatternKind::Literal(expression) => MiddlePattern::Literal(expression),
            PatternKind::Wildcard => MiddlePattern::Wildcard,
            PatternKind::Binding(name) => {
                ctx.declare_variable_known(&name, variable_type);

                MiddlePattern::Binding(name)
            }
            PatternKind::Tuple(ref patterns) => match variable_type {
                DataType::Tuple(data_types) if patterns.len() == data_types.len() => {
                    let patterns = patterns
                        .iter()
                        .cloned()
                        .zip(data_types)
                        .map(|(pattern, data_type)| {
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
            PatternKind::Compound(ref compound) => match variable_type {
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
                            let pattern = pattern
                                .clone()
                                .perform_semantic_analysis(ctx, *data_type.clone())?;

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
            PatternKind::Struct(ref name, ref field_patterns) => match variable_type {
                DataType::Struct(ref struct_name, ref generics) if struct_name == name => {
                    let declaration = ctx.get_data_type(struct_name)??;
                    let field_types = declaration.get_struct_fields(ctx, generics)?;

                    let field_patterns = field_patterns
                        .iter()
                        .map(|(name, pattern)| {
                            let Some(field_type) = field_types.get(&name.snbt_string.1) else {
                                pattern.kind.destructure_unknown(ctx);

                                return ctx.add_error(
                                    name.span,
                                    SemanticAnalysisError::UnexpectedField(
                                        name.snbt_string.1.clone(),
                                    ),
                                );
                            };

                            let field_type = field_type.clone();

                            let pattern =
                                pattern.clone().perform_semantic_analysis(ctx, field_type)?;

                            Some((name.snbt_string.clone(), pattern))
                        })
                        .collect_option_all()?;

                    MiddlePattern::Struct(name.clone(), field_patterns)
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
        })
    }
}
