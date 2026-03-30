use minecraft_command_types::{
    command::enums::difficulty::Difficulty, coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    high::{
        command::{
            data::DataCommand, execute::subcommand::ExecuteSubcommand,
            function::FunctionCommandArguments, r#return::ReturnCommand,
            scoreboard::ScoreboardCommand, stopwatch::StopwatchCommand,
        },
        entity_selector::EntitySelector,
        expression::Expression,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    low::{data_type::DataType, expression::command::Command as MiddleCommand},
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod stopwatch;

#[derive(Debug, Clone)]
pub enum Command {
    Data(DataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(EntitySelector, ResourceLocation, Option<i32>),
    Execute(ExecuteSubcommand),
    Function(
        SupportsExpressionSigil<ResourceLocation>,
        Option<FunctionCommandArguments>,
    ),
    Tellraw(EntitySelector, Expression),
    Return(ReturnCommand),
    Scoreboard(ScoreboardCommand),
    Stopwatch(StopwatchCommand),
    Summon(ResourceLocation, Option<Coordinates>, Option<Expression>),
}

impl Command {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleCommand> {
        Some(match self {
            Self::Data(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleCommand::Data(command)
            }
            Self::Difficulty(difficulty) => MiddleCommand::Difficulty(difficulty),
            Self::Stopwatch(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleCommand::Stopwatch(command)
            }
            Self::Enchant(selector, enchantment, level) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddleCommand::Enchant(selector, enchantment, level)
            }
            Self::Execute(subcommand) => {
                let subcommand = subcommand.perform_semantic_analysis(ctx)?;

                MiddleCommand::Execute(subcommand)
            }
            Self::Function(resource_location, arguments) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(ctx, &DataType::ResourceLocation);

                let arguments = match arguments {
                    Some(arguments) => Some(arguments.perform_semantic_analysis(ctx))?,
                    None => None,
                };

                let resource_location = resource_location?;

                MiddleCommand::Function(resource_location, arguments)
            }
            Self::Tellraw(selector, expression) => {
                let selector = selector.perform_semantic_analysis(ctx);
                let expression = expression.perform_semantic_analysis(ctx);

                let selector = selector?;
                let (_, expression) = expression?;

                MiddleCommand::Tellraw(selector, expression)
            }
            Self::Return(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleCommand::Return(command)
            }
            Self::Scoreboard(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleCommand::Scoreboard(command)
            }
            Self::Summon(resource_location, coordinates, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                MiddleCommand::Summon(resource_location, coordinates, expression)
            }
        })
    }
}
