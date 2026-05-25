use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleScoreboardCommand::Players(command)
            }
        })
    }
}
