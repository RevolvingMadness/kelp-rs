use minecraft_command_types::{
    command::enums::difficulty::Difficulty, coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    parsed::{
        command::{
            data::ParsedDataCommand, execute::subcommand::ParsedExecuteSubcommand,
            function::FunctionCommandArguments, r#return::ReturnCommand,
            scoreboard::ScoreboardCommand, stopwatch::ParsedStopwatchCommand,
        },
        entity_selector::ParsedEntitySelector,
        expression::ParsedExpression,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    semantic::expression::command::SemanticCommand,
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod stopwatch;

#[derive(Debug, Clone)]
pub enum ParsedCommand {
    Data(ParsedDataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(
        ParsedSupportsExpressionSigil<ParsedEntitySelector>,
        ResourceLocation,
        Option<i32>,
    ),
    Execute(ParsedExecuteSubcommand),
    Function(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<FunctionCommandArguments>,
    ),
    Tellraw(
        ParsedSupportsExpressionSigil<ParsedEntitySelector>,
        ParsedExpression,
    ),
    Return(ReturnCommand),
    Scoreboard(ScoreboardCommand),
    Stopwatch(ParsedStopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<ParsedExpression>,
    ),
}

impl ParsedCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticCommand> {
        Some(match self {
            Self::Data(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticCommand::Data(command)
            }
            Self::Difficulty(difficulty) => SemanticCommand::Difficulty(difficulty),
            Self::Stopwatch(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticCommand::Stopwatch(command)
            }
            Self::Enchant(selector, enchantment, level) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                SemanticCommand::Enchant(selector, enchantment, level)
            }
            Self::Execute(subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx)?;

                SemanticCommand::Execute(subcommand)
            }
            Self::Function(resource_location, arguments) => {
                let resource_location = resource_location.perform_semantic_analysis(ctx);

                let arguments = match arguments {
                    Some(arguments) => Some(arguments.perform_semantic_analysis(ctx))?,
                    None => None,
                };

                let resource_location = resource_location?;

                SemanticCommand::Function(resource_location, arguments)
            }
            Self::Tellraw(selector, expression) => {
                let selector = selector.perform_semantic_analysis(ctx);
                let expression = expression.perform_semantic_analysis(ctx);

                let selector = selector?;
                let (_, expression) = expression?;

                SemanticCommand::Tellraw(selector, expression)
            }
            Self::Return(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticCommand::Return(command)
            }
            Self::Scoreboard(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticCommand::Scoreboard(command)
            }
            Self::Summon(resource_location, coordinates, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                SemanticCommand::Summon(resource_location, coordinates, expression)
            }
        })
    }
}
