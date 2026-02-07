use minecraft_command_types::{
    command::{Command, enums::difficulty::Difficulty, r#return::ReturnCommand},
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::Expression,
    high::{
        command::{
            data::HighDataCommand, execute::subcommand::HighExecuteSubcommand,
            function::HighFunctionCommandArguments, r#return::HighReturnCommand,
            scoreboard::HighScoreboardCommand,
        },
        entity_selector::HighEntitySelector,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighCommand {
    Regular(Command),
    Data(HighDataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(HighEntitySelector, ResourceLocation, Option<i32>),
    Execute(HighExecuteSubcommand),
    Function(ResourceLocation, Option<HighFunctionCommandArguments>),
    Tellraw(HighEntitySelector, Expression),
    Return(HighReturnCommand),
    Scoreboard(HighScoreboardCommand),
    Summon(ResourceLocation, Option<Coordinates>, Option<Expression>),
}

impl HighCommand {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match self {
            HighCommand::Regular(_command) => {
                // TODO future

                Some(())
            }
            HighCommand::Data(command) => command.perform_semantic_analysis(ctx),
            HighCommand::Difficulty(_) => Some(()),
            HighCommand::Enchant(selector, _, _) => selector.perform_semantic_analysis(ctx),
            HighCommand::Execute(subcommand) => subcommand.perform_semantic_analysis(ctx),
            HighCommand::Function(_, arguments) => arguments
                .as_ref()
                .map(|arguments| arguments.perform_semantic_analysis(ctx))
                .unwrap_or(Some(())),
            HighCommand::Tellraw(selector, expression) => {
                let selector_result = selector.perform_semantic_analysis(ctx);
                let expression_result = expression.perform_semantic_analysis(ctx);

                selector_result?;
                expression_result?;

                Some(())
            }
            HighCommand::Return(command) => command.perform_semantic_analysis(ctx),
            HighCommand::Scoreboard(command) => command.perform_semantic_analysis(ctx),
            HighCommand::Summon(_, _, expression) => expression
                .as_ref()
                .map(|expression| expression.perform_semantic_analysis(ctx))
                .unwrap_or(Some(())),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Command> {
        match self {
            HighCommand::Regular(command) => Some(command),
            HighCommand::Data(data_command) => data_command.compile(datapack, ctx),
            HighCommand::Difficulty(difficulty) => Some(Command::Difficulty(difficulty)),
            HighCommand::Enchant(selector, location, level) => Some(Command::Enchant(
                selector.compile(datapack, ctx),
                location,
                level,
            )),
            HighCommand::Execute(execute_subcommand) => execute_subcommand
                .compile(datapack, ctx)
                .map(Command::Execute),
            HighCommand::Function(id, arguments) => {
                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(datapack, ctx));
                Some(Command::Function(id, compiled_arguments))
            }
            HighCommand::Tellraw(selector, expression) => {
                let expression = expression.resolve(datapack, ctx);

                Some(Command::Tellraw(
                    selector.compile(datapack, ctx),
                    expression.kind.as_text_component(datapack, ctx, false),
                ))
            }
            HighCommand::Return(command) => match command {
                HighReturnCommand::Fail | HighReturnCommand::Value(0) => {
                    Some(Command::Return(ReturnCommand::Fail))
                }
                HighReturnCommand::Value(value) => {
                    Some(Command::Return(ReturnCommand::Value(value)))
                }
                HighReturnCommand::Run(command) => {
                    command.compile(datapack, ctx).map(|compiled_command| {
                        Command::Return(ReturnCommand::Run(Box::new(compiled_command)))
                    })
                }
            },
            HighCommand::Scoreboard(command) => {
                Some(Command::Scoreboard(command.compile(datapack, ctx)))
            }
            HighCommand::Summon(entity, position, nbt) => Some(Command::Summon(
                entity,
                position,
                nbt.map(|nbt| nbt.resolve(datapack, ctx).kind.as_snbt_macros(ctx)),
            )),
        }
    }
}
