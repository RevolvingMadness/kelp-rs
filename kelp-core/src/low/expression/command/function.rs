use std::collections::HashMap;

use minecraft_command_types::{
    command::function::FunctionCommandArguments as LowFunctionCommandArguments, snbt::SNBTString,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{data::DataTarget, expression::unresolved::UnresolvedExpression, nbt_path::NbtPath},
};

#[derive(Debug, Clone)]
pub enum FunctionCommandArguments {
    Compound(HashMap<SNBTString, UnresolvedExpression>),
    DataTarget(DataTarget, Option<NbtPath>),
}

impl FunctionCommandArguments {
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowFunctionCommandArguments {
        match self {
            Self::Compound(compound) => LowFunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(datapack, ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            Self::DataTarget(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                LowFunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
