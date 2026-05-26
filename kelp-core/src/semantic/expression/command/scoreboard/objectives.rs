use minecraft_command_types::command::{
    enums::scoreboard_render_type::ScoreboardRenderType,
    scoreboard::{ObjectivesScoreboardCommand, ScoreboardModification},
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::expression::{
        SemanticExpression, command::scoreboard::players::SemanticScoreboardNumberFormat,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(SemanticExpression),
    NumberFormat(Option<SemanticScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl SemanticScoreboardModification {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardModification {
        match self {
            Self::DisplayAutoUpdate(auto_update) => {
                ScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let snbt = expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx);

                ScoreboardModification::DisplayName(snbt)
            }
            Self::NumberFormat(number_format) => {
                let number_format =
                    number_format.map(|number_format| number_format.compile(datapack, ctx));

                ScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => ScoreboardModification::RenderType(render_type),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<SemanticExpression>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, SemanticScoreboardModification),
}

impl SemanticObjectivesScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ObjectivesScoreboardCommand {
        match self {
            Self::List => ObjectivesScoreboardCommand::List,
            Self::Add(objective, criterion, expression) => {
                let expression = expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx));

                ObjectivesScoreboardCommand::Add(objective, criterion, expression)
            }
            Self::Remove(objective) => ObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                ObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification = modification.compile(datapack, ctx);

                ObjectivesScoreboardCommand::Modify(objective, modification)
            }
        }
    }
}
