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
    datapack::Datapack,
    middle::expression::{Expression, command::scoreboard::players::ScoreboardNumberFormat},
};

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Expression),
    NumberFormat(Option<ScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl ScoreboardModification {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardModification {
        match self {
            Self::DisplayAutoUpdate(auto_update) => {
                LowScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let snbt = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                LowScoreboardModification::DisplayName(snbt)
            }
            Self::NumberFormat(number_format) => {
                let number_format =
                    number_format.map(|number_format| number_format.compile(datapack, ctx));

                LowScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => LowScoreboardModification::RenderType(render_type),
        }
    }
}

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Expression>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, ScoreboardModification),
}

impl ObjectivesScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowObjectivesScoreboardCommand {
        match self {
            Self::List => LowObjectivesScoreboardCommand::List,
            Self::Add(objective, criterion, expression) => {
                let expression = expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx));

                LowObjectivesScoreboardCommand::Add(objective, criterion, expression)
            }
            Self::Remove(objective) => LowObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                LowObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification = modification.compile(datapack, ctx);

                LowObjectivesScoreboardCommand::Modify(objective, modification)
            }
        }
    }
}
