use std::collections::HashMap;

use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::environment::{
        r#type::r#struct::{regular::HighStructStructId, tuple::HighTupleStructId},
        value::variable::HighVariableId,
    },
    low::{
        data::DataTarget,
        data_type::{resolved::ResolvedDataType, unresolved::UnresolvedDataType},
        environment::r#type::r#struct::{StructStructId, TupleStructId},
        expression::{literal::LiteralExpression, resolved::ResolvedExpression},
        nbt_path::NbtPath,
        player_score::PlayerScore,
    },
};

fn destructure_tuple(
    patterns: Vec<UnresolvedPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: ResolvedDataType,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (ResolvedDataType::Tuple(data_types), ResolvedExpression::Tuple(expressions)) => {
            for ((pattern, expression), data_type) in
                patterns.into_iter().zip(expressions).zip(data_types)
            {
                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Tuple(data_types), score @ ResolvedExpression::Score(..)) => {
            for (pattern, data_type) in patterns.into_iter().zip(data_types) {
                pattern.destructure(datapack, ctx, data_type, score.clone());
            }
        }
        (ResolvedDataType::Tuple(data_types), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (i, (pattern, data_type)) in patterns.into_iter().zip(data_types).enumerate() {
                let expression = ResolvedExpression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Index(Some(SNBT::macroable_integer(i as i32)))),
                )));

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Data(inner_type), value @ ResolvedExpression::Data(..)) => {
            destructure_tuple(patterns, datapack, ctx, *inner_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_compound(
    patterns: HashMap<String, UnresolvedPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: ResolvedDataType,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (
            ResolvedDataType::TypedCompound(data_types),
            ResolvedExpression::Compound(expressions),
        ) => {
            for ((key, pattern), (_, expression)) in patterns.into_iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = data_types.get(&key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Compound(data_type), ResolvedExpression::Compound(expressions)) => {
            for ((_, pattern), (_, expression)) in patterns.into_iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Data(data_type), value @ ResolvedExpression::Data(..)) => {
            destructure_compound(patterns, datapack, ctx, *data_type, value);
        }
        (ResolvedDataType::TypedCompound(data_types), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            for (key, pattern) in patterns {
                let data_type = data_types.get(&key).unwrap().clone();

                let expression = ResolvedExpression::Data(Box::new((
                    target.clone(),
                    path.clone()
                        .with_node(NbtPathNode::Named(SNBTString(false, key), None)),
                )));

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_struct_struct(
    field_patterns: HashMap<String, UnresolvedPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: StructStructId,
    data_type: ResolvedDataType,
    value: ResolvedExpression,
) {
    match (data_type, value) {
        (ResolvedDataType::Struct(..), ResolvedExpression::StructStruct(_, fields)) => {
            let (_, _, declaration) = datapack.get_struct_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let field_value = fields.get(&key).unwrap().clone();
                let data_type = field_types.get(&key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, field_value);
            }
        }
        (ResolvedDataType::Struct(..), ResolvedExpression::Data(target_path)) => {
            let (target, path) = *target_path;

            let (_, _, declaration) = datapack.get_struct_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let data_type = field_types.get(&key).unwrap().clone();

                let field_path = path
                    .clone()
                    .with_node(NbtPathNode::Named(SNBTString(false, key), None));

                let field_value = ResolvedExpression::Data(Box::new((target.clone(), field_path)));

                let data_wrapped_type = ResolvedDataType::Data(Box::new(data_type));

                pattern.destructure(datapack, ctx, data_wrapped_type, field_value);
            }
        }
        (ResolvedDataType::Reference(data_type), value) => {
            destructure_struct_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (ResolvedDataType::Data(data_type), value) => {
            destructure_struct_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (ResolvedDataType::Score(data_type), value) => {
            destructure_struct_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_tuple_struct(
    field_patterns: Vec<UnresolvedPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: TupleStructId,
    value_data_type: ResolvedDataType,
    value: ResolvedExpression,
) {
    match (value, value_data_type) {
        (ResolvedExpression::TupleStruct(_, field_expressions), ResolvedDataType::Struct(..)) => {
            let (_, _, declaration) = datapack.get_tuple_struct_type(id);

            let field_types = declaration.field_types.clone();

            for ((field_pattern, field_expression), field_type) in field_patterns
                .into_iter()
                .zip(field_expressions)
                .zip(field_types)
            {
                field_pattern.destructure(datapack, ctx, field_type, field_expression);
            }
        }
        (ResolvedExpression::Data(target_path), ResolvedDataType::Struct(..)) => {
            let (target, path) = *target_path;

            let (_, _, declaration) = datapack.get_tuple_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (field_index, (field_pattern, field_type)) in
                field_patterns.into_iter().zip(field_types).enumerate()
            {
                let field_path =
                    path.clone()
                        .with_node(NbtPathNode::Index(Some(SNBT::macroable_integer(
                            field_index as i32,
                        ))));
                let field_value = ResolvedExpression::Data(Box::new((target.clone(), field_path)));

                let data_wrapped_type = ResolvedDataType::Data(Box::new(field_type));

                field_pattern.destructure(datapack, ctx, data_wrapped_type, field_value);
            }
        }
        (value, ResolvedDataType::Reference(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (value, ResolvedDataType::Data(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (value, ResolvedDataType::Score(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

#[derive(Debug, Clone)]
pub enum UnresolvedPattern {
    Literal(LiteralExpression),

    Wildcard,
    Binding(HighVariableId),

    Score(PlayerScore),
    Data(Box<(DataTarget, NbtPath)>),

    Tuple(Vec<Self>),
    StructStruct(
        HighStructStructId,
        Vec<UnresolvedDataType>,
        HashMap<String, Self>,
    ),
    TupleStruct(HighTupleStructId, Vec<UnresolvedDataType>, Vec<Self>),

    Compound(HashMap<String, Self>),
}

impl UnresolvedPattern {
    pub fn destructure(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data_type: ResolvedDataType,
        value: ResolvedExpression,
    ) {
        match self {
            Self::Literal(..) | Self::Wildcard => {}
            Self::Binding(id) => {
                datapack.declare_value(id, data_type, value);
            }
            Self::Score(score) => {
                let score = score.compile(datapack, ctx);

                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                value.assign_to_data(datapack, ctx, target, path);
            }
            Self::Tuple(patterns) => {
                destructure_tuple(patterns, datapack, ctx, data_type, value);
            }
            Self::Compound(patterns) => {
                destructure_compound(patterns, datapack, ctx, data_type, value);
            }
            Self::StructStruct(id, generic_types, field_patterns) => {
                let id = UnresolvedDataType::resolve_struct_struct(datapack, id, generic_types);

                destructure_struct_struct(field_patterns, datapack, ctx, id, data_type, value);
            }
            Self::TupleStruct(id, generic_types, field_patterns) => {
                let id = UnresolvedDataType::resolve_tuple_struct(datapack, id, generic_types);

                destructure_tuple_struct(field_patterns, datapack, ctx, id, data_type, value);
            }
        }
    }
}
