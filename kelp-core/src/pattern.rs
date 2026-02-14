use std::collections::BTreeMap;

use minecraft_command_types::nbt_path::NbtPathNode;
use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{
        constant::{ConstantExpression, ConstantExpressionKind},
        literal::LiteralExpression,
    },
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

    Compound(BTreeMap<HighSNBTString, Option<Pattern>>),
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
        }
    }

    pub fn destructure(
        &self,
        datapack: &mut HighDatapack,
        value_data_type: DataTypeKind,
        value: ConstantExpression,
    ) {
        match self {
            PatternKind::Literal(_) => {}
            PatternKind::Wildcard => {}
            PatternKind::Binding(name) => {
                datapack.declare_variable(name, value_data_type, value);
            }
            PatternKind::Tuple(patterns) => {
                if let ConstantExpressionKind::Tuple(expressions) = value.kind
                    && let DataTypeKind::Tuple(data_types) = value_data_type
                {
                    for ((pattern, expression), data_type) in
                        patterns.iter().zip(expressions).zip(data_types)
                    {
                        pattern.kind.destructure(datapack, data_type, expression);
                    }
                } else {
                    unreachable!()
                }
            }
            PatternKind::Compound(patterns) => {
                if let ConstantExpressionKind::Compound(expressions) = &value.kind
                    && let DataTypeKind::TypedCompound(data_types) = value_data_type
                {
                    for (((key, pattern), (_, expression)), (_, data_type)) in
                        patterns.iter().zip(expressions).zip(data_types)
                    {
                        let expression = expression.clone();

                        if let Some(pattern) = pattern {
                            pattern.kind.destructure(datapack, data_type, expression);
                        } else {
                            datapack.declare_variable(&key.snbt_string.1, data_type, expression);
                        }
                    }
                } else if let ConstantExpressionKind::Data(target, path) = &value.kind
                    && let DataTypeKind::Data(data_type) = value_data_type
                {
                    for (key, pattern) in patterns {
                        let data_type = *data_type.clone();
                        let expression = ConstantExpressionKind::Data(
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Named(key.snbt_string.clone(), None)),
                        )
                        .into_dummy_constant_expression();

                        if let Some(pattern) = pattern {
                            pattern.kind.destructure(datapack, data_type, expression);
                        } else {
                            datapack.declare_variable(&key.snbt_string.1, data_type, expression);
                        }
                    }
                } else if let ConstantExpressionKind::Compound(expressions) = &value.kind
                    && let DataTypeKind::Compound(data_type) = value_data_type
                {
                    for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                        let expression = expression.clone();
                        let data_type = *data_type.clone();

                        if let Some(pattern) = pattern {
                            pattern.kind.destructure(datapack, data_type, expression);
                        } else {
                            datapack.declare_variable(&key.snbt_string.1, data_type, expression);
                        }
                    }
                } else {
                    unreachable!()
                }
            }
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub span: ParserRange,
    pub kind: PatternKind,
}

impl Pattern {
    pub fn perform_destructure_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        value_span: ParserRange,
        value_type: DataTypeKind,
    ) -> Option<()> {
        match &self.kind {
            PatternKind::Literal(_) | PatternKind::Wildcard => Some(()),
            PatternKind::Binding(name) => {
                ctx.declare_variable_known(name, value_type);

                Some(())
            }
            PatternKind::Tuple(patterns) => {
                if let DataTypeKind::Tuple(data_types) = value_type {
                    if patterns.len() != data_types.len() {
                        for pattern in patterns {
                            pattern.kind.destructure_unknown(ctx);
                        }

                        return ctx.add_info(SemanticAnalysisInfo {
                            span: value_span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::MismatchedPatternTypes {
                                    expected: DataTypeKind::Tuple(data_types),
                                    actual: self.kind.get_type(),
                                },
                            ),
                        });
                    }

                    for (pattern, data_type) in patterns.iter().zip(data_types) {
                        pattern.perform_destructure_semantic_analysis(ctx, value_span, data_type);
                    }

                    Some(())
                } else {
                    for pattern in patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: value_type,
                                actual: self.kind.get_type(),
                            },
                        ),
                    })
                }
            }
            PatternKind::Compound(patterns) => {
                if let DataTypeKind::TypedCompound(data_types) = &value_type {
                    let mut error = false;

                    for (key, pattern) in patterns.iter() {
                        let data_type = data_types
                            .iter()
                            .find(|(value_key, _)| value_key.1 == key.snbt_string.1)
                            .map(|(_, value)| value.clone());

                        if let Some(data_type) = data_type {
                            if let Some(pattern) = pattern {
                                pattern.perform_destructure_semantic_analysis(
                                    ctx, value_span, data_type,
                                );
                            } else {
                                ctx.declare_variable_known(&key.snbt_string.1, data_type);
                            }
                        } else {
                            ctx.add_info::<()>(SemanticAnalysisInfo {
                                span: key.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::TypeDoesntHaveField {
                                        data_type: value_type.clone(),
                                        field: key.snbt_string.1.clone(),
                                    },
                                ),
                            });

                            if let Some(pattern) = pattern {
                                pattern.kind.destructure_unknown(ctx);
                            } else {
                                ctx.declare_variable_unknown(&key.snbt_string.1);
                            }

                            error = true;
                        }
                    }

                    if error { None } else { Some(()) }
                } else if let DataTypeKind::Compound(data_type) = &value_type {
                    for (key, pattern) in patterns.iter() {
                        let data_type = *data_type.clone();

                        if let Some(pattern) = pattern {
                            pattern
                                .perform_destructure_semantic_analysis(ctx, value_span, data_type);
                        } else {
                            ctx.declare_variable_known(&key.snbt_string.1, data_type);
                        }
                    }

                    Some(())
                } else if let DataTypeKind::Data(data_type) = &value_type {
                    match &**data_type {
                        DataTypeKind::Compound(data_type) => {
                            for (key, pattern) in patterns.iter() {
                                let data_type = *data_type.clone();

                                if let Some(pattern) = pattern {
                                    pattern.perform_destructure_semantic_analysis(
                                        ctx, value_span, data_type,
                                    );
                                } else {
                                    ctx.declare_variable_known(&key.snbt_string.1, data_type);
                                }
                            }

                            Some(())
                        }
                        DataTypeKind::TypedCompound(data_types) => {
                            let mut error = false;

                            for (key, pattern) in patterns.iter() {
                                let data_type = data_types
                                    .iter()
                                    .find(|(value_key, _)| value_key.1 == key.snbt_string.1)
                                    .map(|(_, value)| value.clone());

                                if let Some(data_type) = data_type {
                                    if let Some(pattern) = pattern {
                                        pattern.perform_destructure_semantic_analysis(
                                            ctx, value_span, data_type,
                                        );
                                    } else {
                                        ctx.declare_variable_known(&key.snbt_string.1, data_type);
                                    }
                                } else {
                                    ctx.add_info::<()>(SemanticAnalysisInfo {
                                        span: key.span,
                                        kind: SemanticAnalysisInfoKind::Error(
                                            SemanticAnalysisError::TypeDoesntHaveField {
                                                data_type: value_type.clone(),
                                                field: key.snbt_string.1.clone(),
                                            },
                                        ),
                                    });

                                    if let Some(pattern) = pattern {
                                        pattern.kind.destructure_unknown(ctx);
                                    } else {
                                        ctx.declare_variable_unknown(&key.snbt_string.1);
                                    }

                                    error = true;
                                }
                            }

                            if error { None } else { Some(()) }
                        }
                        _ => {
                            for (key, pattern) in patterns {
                                if let Some(pattern) = pattern {
                                    pattern.kind.destructure_unknown(ctx);
                                } else {
                                    ctx.declare_variable_unknown(&key.snbt_string.1);
                                }
                            }

                            ctx.add_info(SemanticAnalysisInfo {
                                span: self.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::MismatchedPatternTypes {
                                        expected: value_type,
                                        actual: self.kind.get_type(),
                                    },
                                ),
                            })
                        }
                    }
                } else {
                    for (key, pattern) in patterns {
                        if let Some(pattern) = pattern {
                            pattern.kind.destructure_unknown(ctx);
                        } else {
                            ctx.declare_variable_unknown(&key.snbt_string.1);
                        }
                    }

                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: value_type,
                                actual: self.kind.get_type(),
                            },
                        ),
                    })
                }
            }
        }
    }

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
