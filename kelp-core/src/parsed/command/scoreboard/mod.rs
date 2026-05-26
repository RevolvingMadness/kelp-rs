use crate::{
    parsed::{
        command::scoreboard::{
            objectives::ParsedObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::scoreboard::SemanticScoreboardCommand,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum ScoreboardCommand {
    Objectives(ParsedObjectivesScoreboardCommand),
    Players(PlayersScoreboardCommand),
}

impl ScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticScoreboardCommand::Players(command)
            }
        })
    }
}
