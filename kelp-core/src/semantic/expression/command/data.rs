use minecraft_command_types::command::data::{
    DataCommand, DataCommandModification, DataCommandModificationMode,
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        data::SemanticDataTarget, expression::SemanticExpression, nbt_path::SemanticNbtPath,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticDataCommandModification {
    From(SemanticDataTarget, Option<SemanticNbtPath>),
    String(
        SemanticDataTarget,
        Option<SemanticNbtPath>,
        Option<i32>,
        Option<i32>,
    ),
    Value(Box<SemanticExpression>),
}

impl SemanticDataCommandModification {
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        match self {
            Self::From(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                DataCommandModification::From(target.target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                DataCommandModification::String(target.target, path, start, end)
            }
            Self::Value(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let expression_snbt = expression.as_snbt_macros(ctx);

                DataCommandModification::Value(expression_snbt)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticDataCommand {
    Get(
        SemanticDataTarget,
        Option<SemanticNbtPath>,
        Option<NotNan<f32>>,
    ),
    Merge(SemanticDataTarget, Box<SemanticExpression>),
    Modify(
        SemanticDataTarget,
        SemanticNbtPath,
        DataCommandModificationMode,
        Box<SemanticDataCommandModification>,
    ),
    Remove(SemanticDataTarget, SemanticNbtPath),
}

impl SemanticDataCommand {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> DataCommand {
        match self {
            Self::Get(target, path, count) => {
                let compiled_target = target.compile(datapack, ctx);
                let compiled_path = path.map(|path| path.compile(datapack, ctx));

                DataCommand::Get(compiled_target.target, compiled_path, count)
            }
            Self::Merge(target, expression) => {
                let target = target.compile(datapack, ctx);
                let expression = expression.kind.resolve(datapack, ctx);

                let snbt = expression.as_snbt_macros(ctx);

                DataCommand::Merge(target.target, snbt)
            }
            Self::Modify(target, path, mode, modification) => {
                let compiled_modification = modification.compile(datapack, ctx);
                let compiled_target = target.compile(datapack, ctx);
                let compiled_path = path.compile(datapack, ctx);

                DataCommand::Modify(
                    compiled_target.target,
                    compiled_path,
                    mode,
                    compiled_modification,
                )
            }
            Self::Remove(target, path) => {
                let target = target.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                DataCommand::Remove(target.target, path)
            }
        }
    }
}
