use std::collections::HashMap;

use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::expression::{Expression, literal::LiteralExpression},
    middle::data_type::DataType,
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
                pattern.destructure(datapack, ctx, data_type, expression);
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

                pattern.destructure(datapack, ctx, data_type, expression);
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
                let data_type = data_types.get(key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, expression);
            }
        }
        (DataType::Compound(data_type), Expression::Compound(expressions)) => {
            for ((_, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                let expression = expression.clone();
                let data_type = *data_type.clone();

                pattern.destructure(datapack, ctx, data_type, expression);
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
                        .with_node(NbtPathNode::Named(key.clone(), None)),
                )));
                let data_type = data_types.get(key).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, expression);
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
                let field_value = fields.get(&key.1).unwrap().clone();
                let data_type = field_types.get(&key.1).unwrap().clone();

                pattern.destructure(datapack, ctx, data_type, field_value);
            }
        }
        (DataType::Struct(_, generics), Expression::Data(target_path)) => {
            let (target, path) = *target_path;

            let declaration = datapack.get_data_type(name).unwrap();
            let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

            for (key, pattern) in field_patterns {
                let field_path = path
                    .clone()
                    .with_node(NbtPathNode::Named(key.clone(), None));
                let field_value = Expression::Data(Box::new((target.clone(), field_path)));

                let data_type = field_types.get(&key.1).unwrap().clone();

                let data_wrapped_type = DataType::Data(Box::new(data_type));

                pattern.destructure(datapack, ctx, data_wrapped_type, field_value);
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
pub enum Pattern {
    Literal(LiteralExpression),

    Wildcard,
    Binding(String),

    Tuple(Vec<Self>),
    Struct(String, HashMap<SNBTString, Self>),

    Compound(HashMap<SNBTString, Self>),
}

impl Pattern {
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
            Self::Struct(name, field_patterns) => {
                destructure_struct(field_patterns, datapack, ctx, name, data_type, value);
            }
        }
    }
}
