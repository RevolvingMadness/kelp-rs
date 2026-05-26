use std::collections::HashMap;

use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBTString, SNBT},
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        data::Data,
        expression::literal::LiteralExpression,
        player_score::PlayerScore,
    },
};
use crate::low::environment::r#type::r#struct::{RegularStructId, TupleStructId};
use crate::low::data_type::DataType;
use crate::low::expression::Expression;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::{
    r#type::r#struct::{regular::HighRegularStructId, tuple::HighTupleStructId},
    value::variable::HighVariableId,
};

fn destructure_tuple(
    patterns: Vec<SemanticPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (DataType::Tuple(data_types), Expression::Tuple(expressions)) => {
            for ((pattern, expression), data_type) in
                patterns.into_iter().zip(expressions).zip(data_types)
            {
                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Tuple(data_types), score @ Expression::Score(..)) => {
            for (pattern, data_type) in patterns.into_iter().zip(data_types) {
                pattern.destructure(datapack, ctx, data_type, score.clone());
            }
        }
        (DataType::Tuple(data_types), Expression::Data(data)) => {
            for (i, (pattern, data_type)) in patterns.into_iter().zip(data_types).enumerate() {
                let expression =
                    Expression::Data(data.clone().with_path_node(NbtPathNode::Index(
                        Some(SNBT::macroable_integer(i as i32)),
                    )));

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Data(inner_type), value @ Expression::Data(..)) => {
            destructure_tuple(patterns, datapack, ctx, *inner_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_compound(
    patterns: HashMap<String, SemanticPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (
            DataType::TypedCompound(data_types),
            Expression::Compound(expressions),
        ) => {
            for ((key, pattern), (_, expression)) in patterns.into_iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = data_types.get(&key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Compound(data_type), Expression::Compound(expressions)) => {
            for ((_, pattern), (_, expression)) in patterns.into_iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Data(data_type), value @ Expression::Data(..)) => {
            destructure_compound(patterns, datapack, ctx, *data_type, value);
        }
        (DataType::TypedCompound(data_types), Expression::Data(data)) => {
            for (key, pattern) in patterns {
                let data_type = data_types.get(&key).unwrap().clone();

                let expression = Expression::Data(
                    data.clone()
                        .with_path_node(NbtPathNode::Named(SNBTString(false, key), None)),
                );

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_regular_struct(
    field_patterns: HashMap<String, SemanticPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: RegularStructId,
    data_type: DataType,
    value: Expression,
) {
    match (data_type, value) {
        (DataType::Struct(..), Expression::RegularStruct(_, fields)) => {
            let (_, _, declaration) = datapack.get_regular_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let field_value = fields.get(&key).unwrap().clone();
                let data_type = field_types.get(&key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, field_value);
            }
        }
        (DataType::Struct(..), Expression::Data(data)) => {
            let (_, _, declaration) = datapack.get_regular_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (key, pattern) in field_patterns {
                let data_type = field_types.get(&key).unwrap().clone();

                let field_value = Expression::Data(
                    data.clone()
                        .with_path_node(NbtPathNode::Named(SNBTString(false, key), None)),
                );

                let data_wrapped_type = DataType::Data(Box::new(data_type));

                pattern.destructure(datapack, ctx, data_wrapped_type, field_value);
            }
        }
        (DataType::Reference(data_type), value) => {
            destructure_regular_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (DataType::Data(data_type), value) => {
            destructure_regular_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (DataType::Score(data_type), value) => {
            destructure_regular_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

fn destructure_tuple_struct(
    field_patterns: Vec<SemanticPattern>,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: TupleStructId,
    value_data_type: DataType,
    value: Expression,
) {
    match (value, value_data_type) {
        (Expression::TupleStruct(_, field_expressions), DataType::Struct(..)) => {
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
        (Expression::Data(data), DataType::Struct(..)) => {
            let (_, _, declaration) = datapack.get_tuple_struct_type(id);

            let field_types = declaration.field_types.clone();

            for (field_index, (field_pattern, field_type)) in
                field_patterns.into_iter().zip(field_types).enumerate()
            {
                let field_value = Expression::Data(data.clone().with_path_node(
                    NbtPathNode::Index(Some(SNBT::macroable_integer(field_index as i32))),
                ));

                let data_wrapped_type = DataType::Data(Box::new(field_type));

                field_pattern.destructure(datapack, ctx, data_wrapped_type, field_value);
            }
        }
        (value, DataType::Reference(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (value, DataType::Data(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (value, DataType::Score(data_type)) => {
            destructure_tuple_struct(field_patterns, datapack, ctx, id, *data_type, value);
        }
        (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
    }
}

#[derive(Debug, Clone)]
pub enum SemanticPattern {
    Literal(LiteralExpression),

    Wildcard,
    Binding(HighVariableId),

    Score(PlayerScore),
    Data(Data),

    Tuple(Vec<Self>),
    RegularStruct(
        HighRegularStructId,
        Vec<SemanticDataType>,
        HashMap<String, Self>,
    ),
    TupleStruct(HighTupleStructId, Vec<SemanticDataType>, Vec<Self>),

    Compound(HashMap<String, Self>),
}

impl SemanticPattern {
    pub fn destructure(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data_type: DataType,
        value: Expression,
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
            Self::Data(data) => {
                let data = data.compile(datapack, ctx);

                value.assign_to_data(datapack, ctx, data);
            }
            Self::Tuple(patterns) => {
                destructure_tuple(patterns, datapack, ctx, data_type, value);
            }
            Self::Compound(patterns) => {
                destructure_compound(patterns, datapack, ctx, data_type, value);
            }
            Self::RegularStruct(id, generic_types, field_patterns) => {
                let id = SemanticDataType::resolve_regular_struct(datapack, id, generic_types);

                destructure_regular_struct(field_patterns, datapack, ctx, id, data_type, value);
            }
            Self::TupleStruct(id, generic_types, field_patterns) => {
                let id = SemanticDataType::resolve_tuple_struct(datapack, id, generic_types);

                destructure_tuple_struct(field_patterns, datapack, ctx, id, data_type, value);
            }
        }
    }
}
