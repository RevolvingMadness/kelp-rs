use minecraft_command_types::command::enums::scoreboard_render_type::ScoreboardRenderType;

use crate::{
    parsed::{
        command::scoreboard::players::ScoreboardNumberFormat, expression::ParsedExpression,
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::scoreboard::objectives::{
        SemanticObjectivesScoreboardCommand, SemanticScoreboardModification,
    },
};

#[derive(Debug, Clone)]
pub enum ParsedScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Box<ParsedExpression>),
    NumberFormat(Option<Box<ScoreboardNumberFormat>>),
    RenderType(ScoreboardRenderType),
}

impl ParsedScoreboardModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticScoreboardModification> {
        Some(match self {
            Self::DisplayAutoUpdate(auto_update) => {
                SemanticScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticScoreboardModification::DisplayName(expression)
            }
            Self::NumberFormat(number_format) => {
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                SemanticScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => {
                SemanticScoreboardModification::RenderType(render_type)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Box<ParsedExpression>>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ParsedScoreboardModification),
}

impl ParsedObjectivesScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticObjectivesScoreboardCommand> {
        Some(match self {
            Self::List => SemanticObjectivesScoreboardCommand::List,
            Self::Add(name, criterion, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                SemanticObjectivesScoreboardCommand::Add(name, criterion, expression)
            }
            Self::Remove(objective) => SemanticObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                SemanticObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification = modification.perform_semantic_analysis(ctx)?;

                SemanticObjectivesScoreboardCommand::Modify(objective, modification)
            }
        })
    }
}
