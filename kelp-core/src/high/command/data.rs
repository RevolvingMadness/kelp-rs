use minecraft_command_types::command::data::DataCommandModificationMode;
use ordered_float::NotNan;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        data::DataTarget,
        expression::{Expression, ExpressionId},
        nbt_path::NbtPath,
        semantic_analysis::SemanticAnalysisContext,
    },
    low::expression::command::data::{
        DataCommand as MiddleDataCommand, DataCommandModification as MiddleDataCommandModification,
    },
};

#[derive(Debug, Clone)]
pub enum DataCommandModification {
    From(DataTarget, Option<NbtPath>),
    String(DataTarget, Option<NbtPath>, Option<i32>, Option<i32>),
    Value(ExpressionId),
}

impl DataCommandModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataCommandModification> {
        Some(match self {
            Self::From(target, path) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = match path {
                    Some(path) => {
                        Some(path.perform_semantic_analysis(high_allocator, low_allocator, ctx)?)
                    }
                    None => None,
                };

                let target = target?;

                MiddleDataCommandModification::From(target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = match path {
                    Some(path) => {
                        Some(path.perform_semantic_analysis(high_allocator, low_allocator, ctx)?)
                    }
                    None => None,
                };

                let target = target?;

                MiddleDataCommandModification::String(target, path, start, end)
            }
            Self::Value(expression) => {
                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                MiddleDataCommandModification::Value(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum DataCommand {
    Get(DataTarget, Option<NbtPath>, Option<NotNan<f32>>),
    Merge(DataTarget, ExpressionId),
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataCommand> {
        Some(match self {
            Self::Get(target, path, scale) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = match path {
                    Some(path) => {
                        Some(path.perform_semantic_analysis(high_allocator, low_allocator, ctx)?)
                    }
                    None => None,
                };

                let target = target?;

                MiddleDataCommand::Get(target, path, scale)
            }
            Self::Merge(target, expression) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                );

                let target = target?;
                let expression = expression?;

                MiddleDataCommand::Merge(target, expression)
            }
            Self::Modify(target, path, mode, modification) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = path.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let modification =
                    modification.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let target = target?;
                let path = path?;
                let modification = modification?;

                MiddleDataCommand::Modify(target, path, mode, Box::new(modification))
            }
            Self::Remove(target, path) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = path.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let target = target?;
                let path = path?;

                MiddleDataCommand::Remove(target, path)
            }
        })
    }
}
