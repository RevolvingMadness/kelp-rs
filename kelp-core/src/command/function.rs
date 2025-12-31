use std::collections::BTreeMap;

use minecraft_command_types::command::function::FunctionCommandArguments;
use minecraft_command_types_derive::HasMacro;

use crate::{
    command::{context::CompileContext, data::HighDataTarget},
    datapack::HighDatapack,
    expression::{Expression, StringExpression},
    nbt_path::HighNbtPath,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighFunctionCommandArguments {
    Compound(BTreeMap<StringExpression, Expression>),
    DataTarget(HighDataTarget, Option<HighNbtPath>),
}

impl HighFunctionCommandArguments {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> FunctionCommandArguments {
        match self {
            HighFunctionCommandArguments::Compound(compound) => FunctionCommandArguments::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let key = key.compile(datapack, ctx);
                        let value = value.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                        (key, value)
                    })
                    .collect(),
            ),
            HighFunctionCommandArguments::DataTarget(target, path) => {
                let target = target.kind.clone().compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                FunctionCommandArguments::DataTarget(target, path)
            }
        }
    }
}
