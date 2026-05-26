use minecraft_command_types::command::data::{
    DataCommand, DataCommandModification, DataCommandModificationMode,
};
use ordered_float::NotNan;

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
pub enum TypedDataCommandModification {
    From(TypedDataTarget, Option<TypedNbtPath>),
    String(
        TypedDataTarget,
        Option<TypedNbtPath>,
        Option<i32>,
        Option<i32>,
    ),
    Value(TypedExpressionId),
}

impl TypedDataCommandModification {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        match self {
            Self::From(target, path) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.map(|path| path.compile(arena, datapack, ctx));

                DataCommandModification::From(target.target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.map(|path| path.compile(arena, datapack, ctx));

                DataCommandModification::String(target.target, path, start, end)
            }
            Self::Value(expression) => {
                let expression = TypedExpression::resolve(expression, arena, datapack, ctx);

                let expression_snbt = expression.as_snbt_macros(ctx);

                DataCommandModification::Value(expression_snbt)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedDataCommand {
    Get(TypedDataTarget, Option<TypedNbtPath>, Option<NotNan<f32>>),
    Merge(TypedDataTarget, TypedExpressionId),
    Modify(
        TypedDataTarget,
        TypedNbtPath,
        DataCommandModificationMode,
        Box<TypedDataCommandModification>,
    ),
    Remove(TypedDataTarget, TypedNbtPath),
}

impl TypedDataCommand {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> DataCommand {
        match self {
            Self::Get(target, path, count) => {
                let compiled_target = target.compile(arena, datapack, ctx);
                let compiled_path = path.map(|path| path.compile(arena, datapack, ctx));

                DataCommand::Get(compiled_target.target, compiled_path, count)
            }
            Self::Merge(target, expression) => {
                let target = target.compile(arena, datapack, ctx);
                let expression = TypedExpression::resolve(expression, arena, datapack, ctx);

                let snbt = expression.as_snbt_macros(ctx);

                DataCommand::Merge(target.target, snbt)
            }
            Self::Modify(target, path, mode, modification) => {
                let compiled_modification = modification.compile(arena, datapack, ctx);
                let compiled_target = target.compile(arena, datapack, ctx);
                let compiled_path = path.compile(arena, datapack, ctx);

                DataCommand::Modify(
                    compiled_target.target,
                    compiled_path,
                    mode,
                    compiled_modification,
                )
            }
            Self::Remove(target, path) => {
                let target = target.compile(arena, datapack, ctx);
                let path = path.compile(arena, datapack, ctx);

                DataCommand::Remove(target.target, path)
            }
        }
    }
}
