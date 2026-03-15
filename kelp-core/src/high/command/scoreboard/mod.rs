use minecraft_command_types_derive::HasMacro;

use crate::{
    high::command::scoreboard::{
        objectives::ObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
    },
    middle::expression::command::scoreboard::ScoreboardCommand as MiddleScoreboardCommand,
    semantic_analysis_context::SemanticAnalysisContext,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ScoreboardCommand {
    Objectives(Box<ObjectivesScoreboardCommand>), // TODO: Remove box
    Players(PlayersScoreboardCommand),
}

impl ScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command = command.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command = command.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleScoreboardCommand::Players(command)
            }
        })
    }
}
