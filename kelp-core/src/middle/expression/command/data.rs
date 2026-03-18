use minecraft_command_types::command::data::{
    DataCommand as LowDataCommand, DataCommandModification as LowDataCommandModification,
    DataCommandModificationMode,
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::{data::DataTarget, expression::Expression, nbt_path::NbtPath},
};

#[derive(Debug, Clone, HasMacro)]
pub enum DataCommandModification {
    From(DataTarget, Option<NbtPath>),
    String(DataTarget, Option<NbtPath>, Option<i32>, Option<i32>),
    Value(Box<Expression>),
}

impl DataCommandModification {
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowDataCommandModification {
        match self {
            Self::From(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                LowDataCommandModification::From(target.target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                LowDataCommandModification::String(target.target, path, start, end)
            }
            Self::Value(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let expression_snbt = expression.as_snbt_macros(ctx);

                LowDataCommandModification::Value(expression_snbt)
            }
        }
    }
}

#[derive(Debug, Clone, HasMacro)]
pub enum DataCommand {
    Get(DataTarget, Option<NbtPath>, Option<NotNan<f32>>),
    Merge(DataTarget, Box<Expression>),
    Modify(
        DataTarget,
        NbtPath,
        DataCommandModificationMode,
        Box<DataCommandModification>,
    ),
    Remove(DataTarget, NbtPath),
}

impl DataCommand {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowDataCommand {
        match self {
            Self::Get(target, path, count) => {
                let compiled_target = target.compile(datapack, ctx);
                let compiled_path = path.map(|path| path.compile(datapack, ctx));

                LowDataCommand::Get(compiled_target.target, compiled_path, count)
            }
            Self::Merge(target, expression) => {
                let target = target.compile(datapack, ctx);
                let expression = expression.kind.resolve(datapack, ctx);

                let snbt = expression.as_snbt_macros(ctx);

                LowDataCommand::Merge(target.target, snbt)
            }
            Self::Modify(target, path, mode, modification) => {
                let compiled_modification = modification.compile(datapack, ctx);
                let compiled_target = target.compile(datapack, ctx);
                let compiled_path = path.compile(datapack, ctx);

                LowDataCommand::Modify(
                    compiled_target.target,
                    compiled_path,
                    mode,
                    compiled_modification,
                )
            }
            Self::Remove(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                LowDataCommand::Remove(target.target, path)
            }
        }
    }
}
