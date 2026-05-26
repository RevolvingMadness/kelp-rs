use minecraft_command_types::command::data::DataCommandModificationMode;
use ordered_float::NotNan;

use crate::{
    parsed::{
        data::ParsedDataTarget, expression::ParsedExpression, nbt_path::NbtPath,
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::data::{SemanticDataCommand, SemanticDataCommandModification},
};

#[derive(Debug, Clone)]
pub enum ParsedDataCommandModification {
    From(ParsedDataTarget, Option<NbtPath>),
    String(ParsedDataTarget, Option<NbtPath>, Option<i32>, Option<i32>),
    Value(Box<ParsedExpression>),
}

impl ParsedDataCommandModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticDataCommandModification> {
        Some(match self {
            Self::From(target, path) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;

                SemanticDataCommandModification::From(target, path)
            }
            Self::String(target, path, start, end) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;

                SemanticDataCommandModification::String(target, path, start, end)
            }
            Self::Value(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticDataCommandModification::Value(Box::new(expression))
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedDataCommand {
    Get(ParsedDataTarget, Option<NbtPath>, Option<NotNan<f32>>),
    Merge(ParsedDataTarget, Box<ParsedExpression>),
    Modify(
        ParsedDataTarget,
        NbtPath,
        DataCommandModificationMode,
        Box<ParsedDataCommandModification>,
    ),
    Remove(ParsedDataTarget, NbtPath),
}

impl ParsedDataCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticDataCommand> {
        Some(match self {
            Self::Get(target, path, scale) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = match path {
                    Some(path) => Some(path.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let target = target?;

                SemanticDataCommand::Get(target, path, scale)
            }
            Self::Merge(target, expression) => {
                let target = target.perform_semantic_analysis(ctx);
                let expression = expression.perform_semantic_analysis(ctx);

                let target = target?;
                let (_, expression) = expression?;

                SemanticDataCommand::Merge(target, Box::new(expression))
            }
            Self::Modify(target, path, mode, modification) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);
                let modification = modification.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;
                let modification = modification?;

                SemanticDataCommand::Modify(target, path, mode, Box::new(modification))
            }
            Self::Remove(target, path) => {
                let target = target.perform_semantic_analysis(ctx);
                let path = path.perform_semantic_analysis(ctx);

                let target = target?;
                let path = path?;

                SemanticDataCommand::Remove(target, path)
            }
        })
    }
}
