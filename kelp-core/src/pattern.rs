use std::collections::HashMap;

use minecraft_command_types::{nbt_path::NbtPathNode, snbt::SNBT};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::snbt_string::SNBTString,
    low::expression::{Expression, literal::LiteralExpression},
    middle::data_type::DataType,
    pattern_type::PatternType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
};

fn destructure_tuple(
    patterns: &[Pattern],
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (DataType::Tuple(data_types), Expression::Tuple(expressions)) => {
            for ((pattern, expression), data_type) in
                patterns.iter().zip(expressions).zip(data_types)
            {
                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Tuple(data_types), Expression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (i, (pattern, data_type)) in patterns.iter().zip(data_types).enumerate() {
                let expression = Expression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Index(Some(SNBT::Integer(i as i32)))),
                )));

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Data(inner_type), value @ Expression::Data(_)) => {
            destructure_tuple(patterns, datapack, ctx, *inner_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_compound(
    patterns: &HashMap<SNBTString, Pattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (DataType::TypedCompound(data_types), Expression::Compound(expressions)) => {
            for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Compound(data_type), Expression::Compound(expressions)) => {
            for ((_, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Data(data_type), value @ Expression::Data(_)) => {
            destructure_compound(patterns, datapack, ctx, *data_type, value);
        }
        (DataType::TypedCompound(data_types), Expression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (key, pattern) in patterns {
                let expression = Expression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Named(key.snbt_string.clone(), None)),
                )));
                let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, expression);
            }
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_struct(
    field_patterns: &HashMap<SNBTString, Pattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    name: &str,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (DataType::Struct(_, generics), Expression::Struct(_, _, fields)) => {
            let declaration = datapack.get_data_type(name).unwrap();
            let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

            for (key, pattern) in field_patterns {
                let field_value = fields.get(&key.snbt_string.1).unwrap().clone();
                let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                pattern
                    .kind
                    .destructure(datapack, ctx, data_type, field_value);
            }
        }
        (DataType::Struct(_, generics), Expression::Data(target_path)) => {
            let (target, path) = *target_path;

            let declaration = datapack.get_data_type(name).unwrap();
            let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

            for (key, pattern) in field_patterns {
                let field_path = path
                    .clone()
                    .with_node(NbtPathNode::Named(key.snbt_string.clone(), None));
                let field_value = Expression::Data(Box::new((target.clone(), field_path)));

                let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                let data_wrapped_type = DataType::Data(Box::new(data_type));

                pattern
                    .kind
                    .destructure(datapack, ctx, data_wrapped_type, field_value);
            }
        }
        (DataType::Reference(data_type), value) => {
            destructure_struct(
                field_patterns,
                datapack,
                ctx,
                name,
                data_type.distribute_references(),
                value,
            );
        }
        (DataType::Data(data_type), value) => {
            destructure_struct(
                field_patterns,
                datapack,
                ctx,
                name,
                data_type.distribute_data(),
                value,
            );
        }
        (DataType::Score(data_type), value) => {
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
    Struct(String, HashMap<SNBTString, Pattern>),

    Compound(HashMap<SNBTString, Pattern>),

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
                    .map(|(key, pattern)| (key.clone(), pattern.kind.get_type()))
                    .collect(),
            ),
            Self::Dereference(pattern) => {
                PatternType::Dereference(Box::new(pattern.kind.get_type()))
            }
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
            Self::Dereference(pattern) => {
                pattern.kind.destructure_unknown(ctx);
            }
            Self::Struct(_, field_patterns) => {
                for pattern in field_patterns.values() {
                    pattern.kind.destructure_unknown(ctx);
                }
            }
        }
    }

    pub fn destructure(
        &self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data_type: DataType,
        value: Expression,
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
