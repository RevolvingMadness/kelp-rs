use minecraft_command_types::command::data::DataCommandModificationMode;
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    high::{data::DataTarget, expression::Expression, nbt_path::NbtPath},
    middle::expression::command::data::{
        DataCommand as MiddleDataCommand, DataCommandModification as MiddleDataCommandModification,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum DataCommandModification {
    From(DataTarget, Option<NbtPath>),
    String(DataTarget, Option<NbtPath>, Option<i32>, Option<i32>),
    Value(Box<Expression>),
}

impl DataCommandModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleDataCommandModification> {
        Some(match self {
            Self::From(target, path) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx, is_lhs)?),
                    None => None,
                };

                let target = target?;

                MiddleDataCommandModification::From(target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx, is_lhs)?),
                    None => None,
                };

                let target = target?;

                MiddleDataCommandModification::String(target, path, start, end)
            }
            Self::Value(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleDataCommandModification::Value(Box::new(expression))
            }
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
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
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleDataCommand> {
        Some(match self {
            Self::Get(target, path, scale) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx, is_lhs)?),
                    None => None,
                };

                let target = target?;

                MiddleDataCommand::Get(target, path, scale)
            }
            Self::Merge(target, expression) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let expression = expression.perform_semantic_analysis(ctx, is_lhs);

                let target = target?;
                let (_, expression) = expression?;

                MiddleDataCommand::Merge(target, Box::new(expression))
            }
            Self::Modify(target, path, mode, modification) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = path.perform_semantic_analysis(ctx, is_lhs);
                let modification = modification.perform_semantic_analysis(ctx, is_lhs);

                let target = target?;
                let path = path?;
                let modification = modification?;

                MiddleDataCommand::Modify(target, path, mode, Box::new(modification))
            }
            Self::Remove(target, path) => {
                let target = target.perform_semantic_analysis(ctx, is_lhs);
                let path = path.perform_semantic_analysis(ctx, is_lhs);

                let target = target?;
                let path = path?;

                MiddleDataCommand::Remove(target, path)
            }
        })
    }
}
