use std::collections::BTreeMap;

use minecraft_command_types::{nbt_path::NbtPathNode, snbt::SNBT};

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{constant::ResolvedExpression, literal::LiteralExpression},
    high::snbt_string::HighSNBTString,
    pattern_type::PatternType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
};

fn destructure_tuple(
    patterns: &[Pattern],
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    data_type: DataTypeKind,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (DataTypeKind::Tuple(data_types), ResolvedExpression::Tuple(expressions)) => {
            for ((pattern, expression), data_type) in
                patterns.iter().zip(expressions).zip(data_types)
            {
                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataTypeKind::Tuple(data_types), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (i, (pattern, data_type)) in patterns.iter().zip(data_types).enumerate() {
                let expression = ResolvedExpression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Index(Some(SNBT::Integer(i as i32)))),
                )));

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataTypeKind::Data(inner_type), value @ ResolvedExpression::Data(_)) => {
            destructure_tuple(patterns, datapack, ctx, *inner_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_compound(
    patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    data_type: DataTypeKind,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (DataTypeKind::TypedCompound(data_types), ResolvedExpression::Compound(expressions)) => {
            for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                if let Some(pattern) = pattern {
                    pattern
                        .kind
                        .destructure(datapack, ctx, data_type, expression);
                } else {
                    datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                }
            }
        }
        (DataTypeKind::Compound(data_type), ResolvedExpression::Compound(expressions)) => {
            for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                if let Some(pattern) = pattern {
                    pattern
                        .kind
                        .destructure(datapack, ctx, data_type, expression);
                } else {
                    datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                }
            }
        }
        (DataTypeKind::Data(data_type), value @ ResolvedExpression::Data(_)) => {
            destructure_compound(patterns, datapack, ctx, *data_type, value);
        }
        (DataTypeKind::TypedCompound(data_types), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (key, pattern) in patterns {
                let expression = ResolvedExpression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Named(key.snbt_string.clone(), None)),
                )));
                let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                if let Some(pattern) = pattern {
                    pattern
                        .kind
                        .destructure(datapack, ctx, data_type, expression);
                } else {
                    datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                }
            }
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_struct(
    field_patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    name: &str,
    data_type: DataTypeKind,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (DataTypeKind::Struct(_, generics), ResolvedExpression::Struct(_, _, fields)) => {
            let declaration = datapack.get_data_type(name).unwrap();
            let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

            for (key, pattern_opt) in field_patterns {
                let field_value = fields.get(&key.snbt_string.1).unwrap().clone();
                let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                if let Some(pattern) = pattern_opt {
                    pattern
                        .kind
                        .destructure(datapack, ctx, data_type, field_value);
                } else {
                    datapack.declare_variable(key.snbt_string.1.clone(), data_type, field_value);
                }
            }
        }
        (DataTypeKind::Struct(_, generics), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            let declaration = datapack.get_data_type(name).unwrap();
            let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

            for (key, pattern_opt) in field_patterns {
                let field_path = path
                    .clone()
                    .with_node(NbtPathNode::Named(key.snbt_string.clone(), None));
                let field_value = ResolvedExpression::Data(Box::new((target.clone(), field_path)));

                let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                let data_wrapped_type = DataTypeKind::Data(Box::new(data_type));

                if let Some(pattern) = pattern_opt {
                    pattern
                        .kind
                        .destructure(datapack, ctx, data_wrapped_type, field_value);
                } else {
                    datapack.declare_variable(
                        key.snbt_string.1.clone(),
                        data_wrapped_type,
                        field_value,
                    );
                }
            }
        }
        (DataTypeKind::Reference(data_type), value) => {
            destructure_struct(
                field_patterns,
                datapack,
                ctx,
                name,
                data_type.distribute_references(),
                value,
            );
        }
        (DataTypeKind::Data(data_type), value) => {
            destructure_struct(
                field_patterns,
                datapack,
                ctx,
                name,
                data_type.distribute_data(),
                value,
            );
        }
        (DataTypeKind::Score(data_type), value) => {
            destructure_struct(
                field_patterns,
                datapack,
                ctx,
                name,
                data_type.distribute_score(),
                value,
            );
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

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

    pub fn destructure(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        data_type: DataTypeKind,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Literal(_) | Self::Wildcard => {}
            Self::Binding(name) => {
                datapack.declare_variable(name.clone(), data_type, value);
            }
            Self::Tuple(patterns) => {
                destructure_tuple(patterns, datapack, ctx, data_type, value);
            }
            Self::Compound(patterns) => {
                destructure_compound(patterns, datapack, ctx, data_type, value);
            }
            Self::Dereference(pattern) => {
                let value = value.dereference(datapack, ctx).unwrap();

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type.dereference().unwrap(), value);
            }
            Self::Struct(name, field_patterns) => {
                destructure_struct(field_patterns, datapack, ctx, name, data_type, value);
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
