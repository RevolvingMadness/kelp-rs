use minecraft_command_types::command::enums::scoreboard_render_type::ScoreboardRenderType;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        command::scoreboard::players::ParsedScoreboardNumberFormat,
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::expression::command::scoreboard::objectives::{
        TypedObjectivesScoreboardCommand, TypedScoreboardModification,
    },
};

#[derive(Debug, Clone)]
pub enum ParsedScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(ParsedExpressionId),
    NumberFormat(Option<Box<ParsedScoreboardNumberFormat>>),
    RenderType(ScoreboardRenderType),
}

impl ParsedScoreboardModification {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedScoreboardModification> {
        Some(match self {
            Self::DisplayAutoUpdate(auto_update) => {
                TypedScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                TypedScoreboardModification::DisplayName(expression)
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

                TypedScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => TypedScoreboardModification::RenderType(render_type),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<ParsedExpressionId>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ParsedScoreboardModification),
}

impl ParsedObjectivesScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedObjectivesScoreboardCommand> {
        Some(match self {
            Self::List => TypedObjectivesScoreboardCommand::List,
            Self::Add(name, criterion, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let expression = ParsedExpression::perform_semantic_analysis(
                            expression,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                TypedObjectivesScoreboardCommand::Add(name, criterion, expression)
            }
            Self::Remove(objective) => TypedObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                TypedObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification =
                    modification.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedObjectivesScoreboardCommand::Modify(objective, modification)
            }
        })
    }
}
