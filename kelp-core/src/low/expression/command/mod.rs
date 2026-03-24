use minecraft_command_types::{
    command::{
        Command as LowCommand, enums::difficulty::Difficulty,
        r#return::ReturnCommand as LowReturnCommand, stopwatch::StopwatchCommand,
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        entity_selector::EntitySelector,
        expression::{
            command::{
                data::DataCommand, execute::subcommand::ExecuteSubcommand,
                function::FunctionCommandArguments, r#return::ReturnCommand,
                scoreboard::ScoreboardCommand,
            },
            unresolved::UnresolvedExpression,
        },
    },
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;

#[derive(Debug, Clone)]
pub enum Command {
    Data(DataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(EntitySelector, ResourceLocation, Option<i32>),
    Execute(ExecuteSubcommand),
    Function(ResourceLocation, Option<FunctionCommandArguments>),
    Tellraw(EntitySelector, UnresolvedExpression),
    Return(ReturnCommand),
    Scoreboard(ScoreboardCommand),
    Stopwatch(StopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<UnresolvedExpression>,
    ),
}

impl Command {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> LowCommand {
        match self {
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
