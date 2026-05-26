use minecraft_command_types::{
    command::enums::difficulty::Difficulty, coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        command::{
            data::ParsedDataCommand, execute::subcommand::ParsedExecuteSubcommand,
            function::ParsedFunctionCommandArguments, r#return::ReturnCommand,
            scoreboard::ParsedScoreboardCommand, stopwatch::ParsedStopwatchCommand,
        },
        entity_selector::EntitySelector,
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    typed::arena::TypedAstArena,
    typed::expression::command::TypedCommand,
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod stopwatch;

#[derive(Debug, Clone)]
pub enum Command {
    Data(ParsedDataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(
        ParsedSupportsExpressionSigil<EntitySelector>,
        ResourceLocation,
        Option<i32>,
    ),
    Execute(ParsedExecuteSubcommand),
    Function(
        ParsedSupportsExpressionSigil<ResourceLocation>,
        Option<ParsedFunctionCommandArguments>,
    ),
    Tellraw(
        ParsedSupportsExpressionSigil<EntitySelector>,
        ParsedExpressionId,
    ),
    Return(ReturnCommand),
    Scoreboard(ParsedScoreboardCommand),
    Stopwatch(ParsedStopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<ParsedExpressionId>,
    ),
}

impl Command {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedCommand> {
        Some(match self {
            Self::Data(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Data(command)
            }
            Self::Difficulty(difficulty) => TypedCommand::Difficulty(difficulty),
            Self::Stopwatch(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Stopwatch(command)
            }
            Self::Enchant(selector, enchantment, level) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Enchant(selector, enchantment, level)
            }
            Self::Execute(subcommand) => {
                let subcommand =
                    subcommand.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Execute(subcommand)
            }
            Self::Function(resource_location, arguments) => {
                let resource_location =
                    resource_location.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let arguments = match arguments {
                    Some(arguments) => {
                        Some(arguments.perform_semantic_analysis(parsed_arena, typed_arena, ctx))?
                    }
                    None => None,
                };

                let resource_location = resource_location?;

                TypedCommand::Function(resource_location, arguments)
            }
            Self::Tellraw(selector, expression) => {
                let selector = selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                );

                let selector = selector?;
                let expression = expression?;

                TypedCommand::Tellraw(selector, expression)
            }
            Self::Return(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Return(command)
            }
            Self::Scoreboard(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedCommand::Scoreboard(command)
            }
            Self::Summon(resource_location, coordinates, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        let expression = ParsedExpression::perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                TypedCommand::Summon(resource_location, coordinates, expression)
            }
        })
    }
}
