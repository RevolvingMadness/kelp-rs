use crate::{
    high::{
        command::scoreboard::{
            objectives::ObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    low::expression::command::scoreboard::ScoreboardCommand as MiddleScoreboardCommand,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum ScoreboardCommand {
    Objectives(ObjectivesScoreboardCommand),
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
