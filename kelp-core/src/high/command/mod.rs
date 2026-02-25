use minecraft_command_types::{
    command::{Command, enums::difficulty::Difficulty, r#return::ReturnCommand},
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
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
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Regular(_command) => {
                // TODO future

                Some(())
            }
            Self::Data(command) => command.perform_semantic_analysis(ctx, is_lhs),
            Self::Difficulty(_) => Some(()),
            Self::Enchant(selector, _, _) => selector.perform_semantic_analysis(ctx, is_lhs),
            Self::Execute(subcommand) => subcommand.perform_semantic_analysis(ctx, is_lhs),
            Self::Function(_, arguments) => arguments.as_ref().map_or(Some(()), |arguments| {
                arguments.perform_semantic_analysis(ctx, is_lhs)
            }),
            Self::Tellraw(selector, expression) => {
                let selector_result = selector.perform_semantic_analysis(ctx, is_lhs);
                let expression_result =
                    expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::SNBT));

                selector_result?;
                expression_result?;

                Some(())
            }
            Self::Return(command) => command.perform_semantic_analysis(ctx, is_lhs),
            Self::Scoreboard(command) => command.perform_semantic_analysis(ctx, is_lhs),
            Self::Summon(_, _, expression) => expression.as_ref().map_or(Some(()), |expression| {
                expression.perform_semantic_analysis(
                    ctx,
                    is_lhs,
                    Some(&DataTypeKind::Compound(Box::new(DataTypeKind::SNBT))),
                )
            }),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Command> {
        match self {
            Self::Regular(command) => Some(command),
            Self::Data(data_command) => data_command.compile(datapack, ctx),
            Self::Difficulty(difficulty) => Some(Command::Difficulty(difficulty)),
            Self::Enchant(selector, location, level) => Some(Command::Enchant(
                selector.compile(datapack, ctx),
                location,
                level,
            )),
            Self::Execute(execute_subcommand) => execute_subcommand
                .compile(datapack, ctx)
                .map(Command::Execute),
            Self::Function(id, arguments) => {
                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(datapack, ctx));
                Some(Command::Function(id, compiled_arguments))
            }
            Self::Tellraw(selector, expression) => {
                let expression = expression.resolve(datapack, ctx);

                Some(Command::Tellraw(
                    selector.compile(datapack, ctx),
                    expression.as_text_component(datapack, ctx, false),
                ))
            }
            Self::Return(command) => match command {
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
            Self::Scoreboard(command) => Some(Command::Scoreboard(command.compile(datapack, ctx))),
            Self::Summon(entity, position, nbt) => Some(Command::Summon(
                entity,
                position,
                nbt.map(|nbt| nbt.resolve(datapack, ctx).as_snbt_macros(ctx)),
            )),
        }
    }
}
