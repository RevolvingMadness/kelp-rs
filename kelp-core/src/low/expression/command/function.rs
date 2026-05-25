use std::collections::HashMap;

use minecraft_command_types::{
    command::function::FunctionCommandArguments as LowFunctionCommandArguments, snbt::SNBTString,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        data::DataTarget,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionId},
        nbt_path::NbtPath,
    },
};

#[derive(Debug, Clone)]
pub enum FunctionCommandArguments {
    Compound(HashMap<SNBTString, UnresolvedExpressionId>),
    DataTarget(DataTarget, Option<NbtPath>),
}

impl FunctionCommandArguments {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowFunctionCommandArguments {
        match self {
            Self::Compound(compound) => LowFunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = UnresolvedExpression::resolve(value, allocator, datapack, ctx)
                            .as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            Self::DataTarget(target, path) => {
                let target = target.compile(allocator, datapack, ctx);
                let path = path.map(|path| path.compile(allocator, datapack, ctx));

                LowFunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
