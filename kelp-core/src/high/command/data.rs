use minecraft_command_types::command::{
    Command,
    data::{DataCommand, DataCommandModification, DataCommandModificationMode},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::Expression,
    high::{data::HighDataTarget, nbt_path::HighNbtPath},
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataCommandModification {
    From(HighDataTarget, Option<HighNbtPath>),
    String(
        HighDataTarget,
        Option<HighNbtPath>,
        Option<i32>,
        Option<i32>,
    ),
    Value(Box<Expression>),
}

impl HighDataCommandModification {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighDataCommandModification::From(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path
                    .as_ref()
                    .map(|path| path.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                target_result?;
                path_result?;

                Some(())
            }
            HighDataCommandModification::String(target, path, _, _) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path
                    .as_ref()
                    .map(|path| path.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                target_result?;
                path_result?;

                Some(())
            }
            HighDataCommandModification::Value(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &HighDataTarget,
        path: &HighNbtPath,
    ) -> Option<DataCommandModification> {
        match self {
            HighDataCommandModification::From(target, path) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                Some(DataCommandModification::From(target, path))
            }
            HighDataCommandModification::String(target, path, start, end) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.map(|path| path.compile(datapack, ctx));

                Some(DataCommandModification::String(target, path, start, end))
            }
            HighDataCommandModification::Value(expression) => {
                let target = target.kind.clone().compile(datapack, ctx);
                let path = path.clone().compile(datapack, ctx);

                let expression = expression.resolve(datapack, ctx);

                expression
                    .kind
                    .assign_to_data(datapack, ctx, target.clone(), path.clone());

                None
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataCommand {
    Get(HighDataTarget, Option<HighNbtPath>, Option<NotNan<f32>>),
    Merge(HighDataTarget, Box<Expression>),
    Modify(
        HighDataTarget,
        HighNbtPath,
        DataCommandModificationMode,
        Box<HighDataCommandModification>,
    ),
    Remove(HighDataTarget, HighNbtPath),
}

impl HighDataCommand {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighDataCommand::Get(target, path, _) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path
                    .as_ref()
                    .map(|path| path.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                target_result?;
                path_result?;

                Some(())
            }
            HighDataCommand::Merge(target, expression) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                expression_result?;

                Some(())
            }
            HighDataCommand::Modify(target, path, _, modification) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path.perform_semantic_analysis(ctx, is_lhs);
                let modification_result = modification.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                path_result?;
                modification_result?;

                Some(())
            }
            HighDataCommand::Remove(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                path_result?;

                Some(())
            }
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Command> {
        match self {
            HighDataCommand::Get(target, path, count) => {
                let compiled_target = target.kind.compile(datapack, ctx);
                let compiled_path = path.map(|path| path.compile(datapack, ctx));

                Some(Command::Data(DataCommand::Get(
                    compiled_target,
                    compiled_path,
                    count,
                )))
            }
            HighDataCommand::Merge(target, expression) => {
                let target = target.kind.compile(datapack, ctx);
                let expression = expression.resolve(datapack, ctx);

                let snbt = expression.kind.as_snbt_macros(ctx);

                Some(Command::Data(DataCommand::Merge(target, snbt)))
            }
            HighDataCommand::Modify(target, path, mode, modification) => {
                let compiled_modification = modification.compile(datapack, ctx, &target, &path)?;
                let compiled_target = target.kind.compile(datapack, ctx);
                let compiled_path = path.compile(datapack, ctx);

                Some(Command::Data(DataCommand::Modify(
                    compiled_target,
                    compiled_path,
                    mode,
                    compiled_modification,
                )))
            }
            HighDataCommand::Remove(target, path) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Some(Command::Data(DataCommand::Remove(target, path)))
            }
        }
    }
}
