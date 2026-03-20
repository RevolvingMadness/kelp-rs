use crate::{
    high::{
        command::scoreboard::{
            objectives::ObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
        },
        semantic_analysis_context::SemanticAnalysisContext,
    },
    middle::expression::command::scoreboard::ScoreboardCommand as MiddleScoreboardCommand,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum ScoreboardCommand {
    Objectives(Box<ObjectivesScoreboardCommand>), // TODO: Remove box
    Players(PlayersScoreboardCommand),
}

impl ScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleScoreboardCommand::Players(command)
            }
        })
    }
}
