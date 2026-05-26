use std::collections::HashMap;

use minecraft_command_types::{command::function::FunctionCommandArguments, snbt::SNBTString};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::{
        data::TypedDataTarget,
        expression::typed::{TypedExpression, TypedExpressionId},
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> FunctionCommandArguments {
        match self {
            Self::Compound(compound) => FunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = TypedExpression::resolve(value, allocator, datapack, ctx)
                            .as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            Self::DataTarget(target, path) => {
                let target = target.compile(allocator, datapack, ctx);
                let path = path.map(|path| path.compile(allocator, datapack, ctx));

                FunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
