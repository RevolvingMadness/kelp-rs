use std::collections::BTreeMap;

use minecraft_command_types::{
    command::function::FunctionCommandArguments as LowFunctionCommandArguments, snbt::SNBTString,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::{data::DataTarget, expression::Expression, nbt_path::NbtPath},
};

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum FunctionCommandArguments {
    Compound(BTreeMap<SNBTString, Expression>),
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
                        let value = value.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

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
