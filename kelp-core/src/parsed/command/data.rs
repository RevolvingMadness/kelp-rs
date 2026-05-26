use minecraft_command_types::command::data::DataCommandModificationMode;
use ordered_float::NotNan;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        data::DataTarget,
        expression::{ParsedExpression, ParsedExpressionId},
        nbt_path::ParsedNbtPath,
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::expression::command::data::{TypedDataCommand, TypedDataCommandModification},
};

#[derive(Debug, Clone)]
pub enum ParsedDataCommandModification {
    From(DataTarget, Option<ParsedNbtPath>),
    String(DataTarget, Option<ParsedNbtPath>, Option<i32>, Option<i32>),
    Value(ParsedExpressionId),
}

impl ParsedDataCommandModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedDataCommandModification> {
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

                TypedDataCommandModification::From(target, path)
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

                TypedDataCommandModification::String(target, path, start, end)
            }
            Self::Value(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                TypedDataCommandModification::Value(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedDataCommand {
    Get(DataTarget, Option<ParsedNbtPath>, Option<NotNan<f32>>),
    Merge(DataTarget, ParsedExpressionId),
    Modify(
        DataTarget,
        ParsedNbtPath,
        DataCommandModificationMode,
        Box<ParsedDataCommandModification>,
    ),
    Remove(DataTarget, ParsedNbtPath),
}

impl ParsedDataCommand {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedDataCommand> {
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

                TypedDataCommand::Get(target, path, scale)
            }
            Self::Merge(target, expression) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                );

                let target = target?;
                let expression = expression?;

                TypedDataCommand::Merge(target, expression)
            }
            Self::Modify(target, path, mode, modification) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = path.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let modification =
                    modification.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let target = target?;
                let path = path?;
                let modification = modification?;

                TypedDataCommand::Modify(target, path, mode, Box::new(modification))
            }
            Self::Remove(target, path) => {
                let target = target.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let path = path.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let target = target?;
                let path = path?;

                TypedDataCommand::Remove(target, path)
            }
        })
    }
}
