use std::collections::HashMap;

use minecraft_command_types::{command::function::FunctionCommandArguments, snbt::SNBTString};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        data::SemanticDataTarget, expression::SemanticExpression, nbt_path::SemanticNbtPath,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticFunctionCommandArguments {
    Compound(HashMap<SNBTString, SemanticExpression>),
    DataTarget(SemanticDataTarget, Option<SemanticNbtPath>),
}

impl SemanticFunctionCommandArguments {
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> FunctionCommandArguments {
        match self {
            Self::Compound(compound) => FunctionCommandArguments::Compound(
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

                FunctionCommandArguments::DataTarget(target.target, path)
            }
        }
    }
}
