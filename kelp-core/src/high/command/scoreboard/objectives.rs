use la_arena::Idx;
use minecraft_command_types::command::enums::scoreboard_render_type::ScoreboardRenderType;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        command::scoreboard::players::ScoreboardNumberFormat, expression::Expression,
        semantic_analysis::SemanticAnalysisContext,
    },
    low::expression::command::scoreboard::objectives::{
        ObjectivesScoreboardCommand as MiddleObjectivesScoreboardCommand,
        ScoreboardModification as MiddleScoreboardModification,
    },
};

#[derive(Debug, Clone)]
pub enum ScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Idx<Expression>),
    NumberFormat(Option<Box<ScoreboardNumberFormat>>),
    RenderType(ScoreboardRenderType),
}

impl ScoreboardModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreboardModification> {
        Some(match self {
            Self::DisplayAutoUpdate(auto_update) => {
                MiddleScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                MiddleScoreboardModification::DisplayName(expression)
            }
            Self::NumberFormat(number_format) => {
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(
                        high_allocator,
                        low_allocator,
                        ctx,
                    )?),
                    None => None,
                };

                MiddleScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => MiddleScoreboardModification::RenderType(render_type),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Idx<Expression>>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ScoreboardModification),
}

impl ObjectivesScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleObjectivesScoreboardCommand> {
        Some(match self {
            Self::List => MiddleObjectivesScoreboardCommand::List,
            Self::Add(name, criterion, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let expression = Expression::perform_semantic_analysis(
                            expression,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

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
                let modification =
                    modification.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleObjectivesScoreboardCommand::Modify(objective, modification)
            }
        })
    }
}
