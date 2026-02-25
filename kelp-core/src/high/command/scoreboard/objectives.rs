use minecraft_command_types::command::{
    enums::scoreboard_render_type::ScoreboardRenderType,
    scoreboard::{ObjectivesScoreboardCommand, ScoreboardModification},
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, data_type::DataTypeKind, datapack::HighDatapack,
    expression::Expression, high::command::scoreboard::players::HighScoreboardNumberFormat,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Expression),
    NumberFormat(Option<HighScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl HighScoreboardModification {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::DisplayName(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::SNBT))
            }
            Self::NumberFormat(number_format) => {
                number_format.as_ref().map_or(Some(()), |number_format| {
                    number_format.perform_semantic_analysis(ctx, is_lhs)
                })
            }
            Self::DisplayAutoUpdate(_) | Self::RenderType(_) => Some(()),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardModification {
        match self {
            Self::DisplayAutoUpdate(value) => ScoreboardModification::DisplayAutoUpdate(value),
            Self::DisplayName(expression) => ScoreboardModification::DisplayName(
                expression.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::NumberFormat(number_format) => ScoreboardModification::NumberFormat(
                number_format.map(|number_format| number_format.compile(datapack, ctx)),
            ),
            Self::RenderType(render_type) => ScoreboardModification::RenderType(render_type),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Expression>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, HighScoreboardModification),
}

impl HighObjectivesScoreboardCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Add(_, _, expression) => expression.as_ref().map_or(Some(()), |expression| {
                expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::SNBT))
            }),
            Self::List | Self::Remove(_) | Self::SetDisplay(_, _) => Some(()),
            Self::Modify(_, modification) => modification.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ObjectivesScoreboardCommand {
        match self {
            Self::List => ObjectivesScoreboardCommand::List,
            Self::Add(objective, display_name, expression) => ObjectivesScoreboardCommand::Add(
                objective,
                display_name,
                expression.map(|e| e.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
            Self::Remove(objective) => ObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(objective, display_name) => {
                ObjectivesScoreboardCommand::SetDisplay(objective, display_name)
            }
            Self::Modify(objective, scoreboard_modification) => {
                ObjectivesScoreboardCommand::Modify(
                    objective,
                    scoreboard_modification.compile(datapack, ctx),
                )
            }
        }
    }
}
