use minecraft_command_types::{
    command::{
        Command as LowCommand, enums::difficulty::Difficulty,
        r#return::ReturnCommand as LowReturnCommand, stopwatch::StopwatchCommand,
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::Datapack,
    high::{
        command::{
            data::DataCommand, execute::subcommand::ExecuteSubcommand,
            function::FunctionCommandArguments, r#return::ReturnCommand,
            scoreboard::ScoreboardCommand,
        },
        entity_selector::EntitySelector,
        expression::Expression,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum Command {
    Regular(LowCommand),
    Data(DataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(EntitySelector, ResourceLocation, Option<i32>),
    Execute(ExecuteSubcommand),
    Function(ResourceLocation, Option<FunctionCommandArguments>),
    Tellraw(EntitySelector, Expression),
    Return(ReturnCommand),
    Scoreboard(ScoreboardCommand),
    Stopwatch(StopwatchCommand),
    Summon(ResourceLocation, Option<Coordinates>, Option<Expression>),
}

impl Command {
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
            Self::Difficulty(_) | Self::Stopwatch(_) => Some(()),
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

    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowCommand {
        match self {
            Self::Regular(command) => command,
            Self::Data(data_command) => LowCommand::Data(data_command.compile(datapack, ctx)),
            Self::Difficulty(difficulty) => LowCommand::Difficulty(difficulty),
            Self::Enchant(selector, location, level) => {
                LowCommand::Enchant(selector.compile(datapack, ctx), location, level)
            }
            Self::Execute(execute_subcommand) => {
                LowCommand::Execute(execute_subcommand.compile(datapack, ctx))
            }
            Self::Function(id, arguments) => {
                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(datapack, ctx));
                LowCommand::Function(id, compiled_arguments)
            }
            Self::Tellraw(selector, expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                LowCommand::Tellraw(
                    selector.compile(datapack, ctx),
                    expression.as_text_component(datapack, ctx, false),
                )
            }
            Self::Return(command) => match command {
                ReturnCommand::Fail | ReturnCommand::Value(0) => {
                    LowCommand::Return(LowReturnCommand::Fail)
                }
                ReturnCommand::Value(value) => LowCommand::Return(LowReturnCommand::Value(value)),
                ReturnCommand::Run(command) => LowCommand::Return(LowReturnCommand::Run(Box::new(
                    command.compile(datapack, ctx),
                ))),
            },
            Self::Scoreboard(command) => LowCommand::Scoreboard(command.compile(datapack, ctx)),
            Self::Stopwatch(command) => LowCommand::Stopwatch(command),
            Self::Summon(entity, position, nbt) => LowCommand::Summon(
                entity,
                position,
                nbt.map(|nbt| nbt.kind.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
        }
    }
}
