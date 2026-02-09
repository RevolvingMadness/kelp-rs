use minecraft_command_types::command::{
    enums::scoreboard_render_type::ScoreboardRenderType,
    scoreboard::{ObjectivesScoreboardCommand, ScoreboardModification},
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack, expression::Expression,
    high::command::scoreboard::players::HighScoreboardNumberFormat,
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
            HighScoreboardModification::DisplayAutoUpdate(_) => Some(()),
            HighScoreboardModification::DisplayName(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
            HighScoreboardModification::NumberFormat(number_format) => number_format
                .as_ref()
                .map(|number_format| number_format.perform_semantic_analysis(ctx, is_lhs))
                .unwrap_or(Some(())),
            HighScoreboardModification::RenderType(_) => Some(()),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardModification {
        match self {
            HighScoreboardModification::DisplayAutoUpdate(value) => {
                ScoreboardModification::DisplayAutoUpdate(value)
            }
            HighScoreboardModification::DisplayName(expression) => {
                ScoreboardModification::DisplayName(
                    expression.resolve(datapack, ctx).as_snbt_macros(ctx),
                )
            }
            HighScoreboardModification::NumberFormat(number_format) => {
                ScoreboardModification::NumberFormat(
                    number_format.map(|number_format| number_format.compile(datapack, ctx)),
                )
            }
            HighScoreboardModification::RenderType(render_type) => {
                ScoreboardModification::RenderType(render_type)
            }
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
            HighObjectivesScoreboardCommand::List => Some(()),
            HighObjectivesScoreboardCommand::Add(_, _, expression) => expression
                .as_ref()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .unwrap_or(Some(())),
            HighObjectivesScoreboardCommand::Remove(_) => Some(()),
            HighObjectivesScoreboardCommand::SetDisplay(_, _) => Some(()),
            HighObjectivesScoreboardCommand::Modify(_, modification) => {
                modification.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ObjectivesScoreboardCommand {
        match self {
            HighObjectivesScoreboardCommand::List => ObjectivesScoreboardCommand::List,
            HighObjectivesScoreboardCommand::Add(objective, display_name, expression) => {
                ObjectivesScoreboardCommand::Add(
                    objective,
                    display_name,
                    expression.map(|e| e.resolve(datapack, ctx).as_snbt_macros(ctx)),
                )
            }
            HighObjectivesScoreboardCommand::Remove(objective) => {
                ObjectivesScoreboardCommand::Remove(objective)
            }
            HighObjectivesScoreboardCommand::SetDisplay(objective, display_name) => {
                ObjectivesScoreboardCommand::SetDisplay(objective, display_name)
            }
            HighObjectivesScoreboardCommand::Modify(objective, scoreboard_modification) => {
                ObjectivesScoreboardCommand::Modify(
                    objective,
                    scoreboard_modification.compile(datapack, ctx),
                )
            }
        }
    }
}
