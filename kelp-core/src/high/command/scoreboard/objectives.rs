use minecraft_command_types::command::enums::scoreboard_render_type::ScoreboardRenderType;
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::{command::scoreboard::players::ScoreboardNumberFormat, expression::Expression},
    middle::expression::command::scoreboard::objectives::{
        ObjectivesScoreboardCommand as MiddleObjectivesScoreboardCommand,
        ScoreboardModification as MiddleScoreboardModification,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Expression),
    NumberFormat(Option<ScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl ScoreboardModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleScoreboardModification> {
        Some(match self {
            Self::DisplayAutoUpdate(auto_update) => {
                MiddleScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleScoreboardModification::DisplayName(expression)
            }
            Self::NumberFormat(number_format) => {
                let number_format = match number_format {
                    Some(number_format) => {
                        Some(number_format.perform_semantic_analysis(ctx, is_lhs)?)
                    }
                    None => None,
                };

                MiddleScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => MiddleScoreboardModification::RenderType(render_type),
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Expression>), // TODO: box
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ScoreboardModification), // TODO: box
}

impl ObjectivesScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleObjectivesScoreboardCommand> {
        Some(match self {
            Self::List => MiddleObjectivesScoreboardCommand::List,
            Self::Add(name, criterion, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                        Some(expression)
                    }
                    None => None,
                };

                MiddleObjectivesScoreboardCommand::Add(name, criterion, expression)
            }
            Self::Remove(objective) => MiddleObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                MiddleObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification = modification.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleObjectivesScoreboardCommand::Modify(objective, modification)
            }
        })
    }
}
