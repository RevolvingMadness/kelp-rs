use std::collections::HashMap;

use la_arena::Idx;
use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    parsed::environment::resolved::{
        r#type::r#struct::{regular::HighRegularStructId, tuple::HighTupleStructId},
        value::variable::HighVariableId,
    },
    typed::{
        data::TypedData,
        data_type::{resolved::ResolvedDataType, unresolved::UnresolvedDataType},
        environment::r#type::r#struct::{RegularStructId, TupleStructId},
        expression::{literal::LiteralExpression, resolved::Expression},
        player_score::TypedPlayerScore,
    },
};

fn destructure_tuple(
    allocator: &LowAstAllocator,
    patterns: &[Idx<TypedPattern>],
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: ResolvedDataType,
    value: Expression,
) {
    match (data_type, value) {
        (ResolvedDataType::Tuple(data_types), Expression::Tuple(expressions)) => {
            for ((pattern, expression), data_type) in
                patterns.iter().copied().zip(expressions).zip(data_types)
            {
                TypedPattern::destructure(pattern, allocator, datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Tuple(data_types), score @ Expression::Score(..)) => {
            for (pattern, data_type) in patterns.iter().copied().zip(data_types) {
                TypedPattern::destructure(
                    pattern,
                    allocator,
                    datapack,
                    ctx,
                    data_type,
                    score.clone(),
                );
            }
        }
        (ResolvedDataType::Tuple(data_types), Expression::Data(data)) => {
            for (i, (pattern, data_type)) in patterns.iter().copied().zip(data_types).enumerate() {
                let expression =
                    Expression::Data(data.clone().with_path_node(NbtPathNode::Index(Some(
                        SNBT::macroable_integer(i as i32),
                    ))));

                TypedPattern::destructure(pattern, allocator, datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Data(inner_type), value @ Expression::Data(..)) => {
            destructure_tuple(allocator, patterns, datapack, ctx, *inner_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_compound(
    allocator: &LowAstAllocator,
    patterns: &HashMap<String, Idx<TypedPattern>>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: ResolvedDataType,
    value: Expression,
) {
    match (data_type, value) {
        (ResolvedDataType::TypedCompound(data_types), Expression::Compound(expressions)) => {
            for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = data_types.get(key).unwrap().clone();

                TypedPattern::destructure(
                    *pattern, allocator, datapack, ctx, data_type, expression,
                );
            }
        }
        (ResolvedDataType::Compound(data_type), Expression::Compound(expressions)) => {
            for (pattern, expression) in patterns.values().copied().zip(expressions.values()) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                TypedPattern::destructure(pattern, allocator, datapack, ctx, data_type, expression);
            }
        }
        (ResolvedDataType::Data(data_type), value @ Expression::Data(..)) => {
            destructure_compound(allocator, patterns, datapack, ctx, *data_type, value);
        }
        (ResolvedDataType::TypedCompound(data_types), Expression::Data(data)) => {
            for (key, pattern) in patterns {
                let data_type = data_types.get(key).unwrap().clone();

                let expression = Expression::Data(
                    data.clone()
                        .with_path_node(NbtPathNode::Named(SNBTString(false, key.clone()), None)),
                );

                TypedPattern::destructure(
                    *pattern, allocator, datapack, ctx, data_type, expression,
                );
            }
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_regular_struct(
    allocator: &LowAstAllocator,
    field_patterns: &HashMap<String, Idx<TypedPattern>>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: RegularStructId,
    data_type: ResolvedDataType,
    value: Expression,
) {
    match (data_type, value) {
        (ResolvedDataType::Struct(..), Expression::RegularStruct(_, fields)) => {
            let (_, _, declaration) = datapack.get_regular_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let field_value = fields.get(key).unwrap().clone();
                let data_type = field_types.get(key).unwrap().clone();

                TypedPattern::destructure(
                    *pattern,
                    allocator,
                    datapack,
                    ctx,
                    data_type,
                    field_value,
                );
            }
        }
        (ResolvedDataType::Struct(..), Expression::Data(data)) => {
            let (_, _, declaration) = datapack.get_regular_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let data_type = field_types.get(key).unwrap().clone();

                let field_value = Expression::Data(
                    data.clone()
                        .with_path_node(NbtPathNode::Named(SNBTString(false, key.clone()), None)),
                );

                let data_wrapped_type = ResolvedDataType::Data(Box::new(data_type));

                TypedPattern::destructure(
                    *pattern,
                    allocator,
                    datapack,
                    ctx,
                    data_wrapped_type,
                    field_value,
                );
            }
        }
        (ResolvedDataType::Reference(data_type), value) => {
            destructure_regular_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (ResolvedDataType::Data(data_type), value) => {
            destructure_regular_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (ResolvedDataType::Score(data_type), value) => {
            destructure_regular_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_tuple_struct(
    allocator: &LowAstAllocator,
    field_patterns: &[Idx<TypedPattern>],
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: TupleStructId,
    value_data_type: ResolvedDataType,
    value: Expression,
) {
    match (value, value_data_type) {
        (Expression::TupleStruct(_, field_expressions), ResolvedDataType::Struct(..)) => {
            let (_, _, declaration) = datapack.get_tuple_struct_type(id);

            let field_types = declaration.field_types.clone();

            for ((field_pattern, field_expression), field_type) in field_patterns
                .iter()
                .copied()
                .zip(field_expressions)
                .zip(field_types)
            {
                TypedPattern::destructure(
                    field_pattern,
                    allocator,
                    datapack,
                    ctx,
                    field_type,
                    field_expression,
                );
            }
        }
        (Expression::Data(data), ResolvedDataType::Struct(..)) => {
            let (_, _, declaration) = datapack.get_tuple_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (field_index, (field_pattern, field_type)) in
                field_patterns.iter().copied().zip(field_types).enumerate()
            {
                let field_value = Expression::Data(data.clone().with_path_node(
                    NbtPathNode::Index(Some(SNBT::macroable_integer(field_index as i32))),
                ));

                let data_wrapped_type = ResolvedDataType::Data(Box::new(field_type));

                TypedPattern::destructure(
                    field_pattern,
                    allocator,
                    datapack,
                    ctx,
                    data_wrapped_type,
                    field_value,
                );
            }
        }
        (value, ResolvedDataType::Reference(data_type)) => {
            destructure_tuple_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (value, ResolvedDataType::Data(data_type)) => {
            destructure_tuple_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (value, ResolvedDataType::Score(data_type)) => {
            destructure_tuple_struct(
                allocator,
                field_patterns,
                datapack,
                ctx,
                id,
                *data_type,
                value,
            );
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

#[derive(Debug, Clone)]
pub enum TypedPattern {
    Literal(LiteralExpression),

    Wildcard,
    Binding(HighVariableId),

    Score(TypedPlayerScore),
    Data(TypedData),

    Tuple(Vec<Idx<Self>>),
    RegularStruct(
        HighRegularStructId,
        Vec<UnresolvedDataType>,
        HashMap<String, Idx<Self>>,
    ),
    TupleStruct(HighTupleStructId, Vec<UnresolvedDataType>, Vec<Idx<Self>>),

    Compound(HashMap<String, Idx<Self>>),
}

impl TypedPattern {
    pub fn destructure(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data_type: ResolvedDataType,
        value: Expression,
    ) {
        match allocator.get_pattern(id) {
            Self::Literal(..) | Self::Wildcard => {}
            Self::Binding(id) => {
                datapack.declare_value(*id, data_type, value);
            }
            Self::Score(score) => {
                let score = score.clone().compile(allocator, datapack, ctx);

                value.assign_to_score(datapack, ctx, score);
            }
            Self::Data(data) => {
                let data = data.clone().compile(allocator, datapack, ctx);

                value.assign_to_data(datapack, ctx, data);
            }
            Self::Tuple(patterns) => {
                destructure_tuple(allocator, patterns, datapack, ctx, data_type, value);
            }
            Self::Compound(patterns) => {
                destructure_compound(allocator, patterns, datapack, ctx, data_type, value);
            }
            Self::RegularStruct(id, generic_types, field_patterns) => {
                let id = UnresolvedDataType::resolve_regular_struct(
                    datapack,
                    *id,
                    generic_types.clone(),
                );

                destructure_regular_struct(
                    allocator,
                    field_patterns,
                    datapack,
                    ctx,
                    id,
                    data_type,
                    value,
                );
            }
            Self::TupleStruct(id, generic_types, field_patterns) => {
                let id =
                    UnresolvedDataType::resolve_tuple_struct(datapack, *id, generic_types.clone());

                destructure_tuple_struct(
                    allocator,
                    field_patterns,
                    datapack,
                    ctx,
                    id,
                    data_type,
                    value,
                );
            }
        }
    }
}
