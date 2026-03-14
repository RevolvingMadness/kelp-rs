use minecraft_command_types::command::{
    enums::scoreboard_render_type::ScoreboardRenderType,
    scoreboard::{
        ObjectivesScoreboardCommand as LowObjectivesScoreboardCommand,
        ScoreboardModification as LowScoreboardModification,
    },
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::Datapack,
    high::{command::scoreboard::players::ScoreboardNumberFormat, expression::Expression},
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
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardModification {
        match self {
            Self::DisplayAutoUpdate(value) => LowScoreboardModification::DisplayAutoUpdate(value),
            Self::DisplayName(expression) => LowScoreboardModification::DisplayName(
                expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::NumberFormat(number_format) => LowScoreboardModification::NumberFormat(
                number_format.map(|number_format| number_format.compile(datapack, ctx)),
            ),
            Self::RenderType(render_type) => LowScoreboardModification::RenderType(render_type),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Expression>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ScoreboardModification),
}

impl ObjectivesScoreboardCommand {
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
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowObjectivesScoreboardCommand {
        match self {
            Self::List => LowObjectivesScoreboardCommand::List,
            Self::Add(objective, display_name, expression) => LowObjectivesScoreboardCommand::Add(
                objective,
                display_name,
                expression.map(|e| e.kind.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
            Self::Remove(objective) => LowObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(objective, display_name) => {
                LowObjectivesScoreboardCommand::SetDisplay(objective, display_name)
            }
            Self::Modify(objective, scoreboard_modification) => {
                LowObjectivesScoreboardCommand::Modify(
                    objective,
                    scoreboard_modification.compile(datapack, ctx),
                )
            }
        }
    }
}
