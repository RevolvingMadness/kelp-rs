use std::collections::HashMap;

use minecraft_command_types::{command::function::FunctionCommandArguments, snbt::SNBTString};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::{
        data::TypedDataTarget,
        expression::{TypedExpression, TypedExpressionId},
        nbt_path::TypedNbtPath,
    },
};

#[derive(Debug, Clone)]
pub enum TypedFunctionCommandArguments {
    Compound(HashMap<SNBTString, TypedExpressionId>),
    DataTarget(TypedDataTarget, Option<TypedNbtPath>),
}

impl TypedFunctionCommandArguments {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> FunctionCommandArguments {
        match self {
            Self::Compound(compound) => FunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = TypedExpression::resolve(value, arena, datapack, ctx)
                            .as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            Self::DataTarget(target, path) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.map(|path| path.compile(arena, datapack, ctx));

                FunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
