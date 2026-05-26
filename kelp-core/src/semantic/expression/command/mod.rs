use minecraft_command_types::{
    command::{Command, enums::difficulty::Difficulty, r#return::ReturnCommand},
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::{
        entity_selector::SemanticEntitySelector,
        expression::{
            SemanticExpression,
            command::{
                data::DataCommand, execute::subcommand::SemanticExecuteSubcommand,
                function::SemanticFunctionCommandArguments, r#return::SemanticReturnCommand,
                scoreboard::SemanticScoreboardCommand, stopwatch::SemanticStopwatchCommand,
            },
        },
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod stopwatch;

#[derive(Debug, Clone)]
pub enum SemanticCommand {
    Data(DataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(
        SemanticSupportsExpressionSigil<SemanticEntitySelector>,
        ResourceLocation,
        Option<i32>,
    ),
    Execute(SemanticExecuteSubcommand),
    Function(
        SemanticSupportsExpressionSigil<ResourceLocation>,
        Option<SemanticFunctionCommandArguments>,
    ),
    Tellraw(
        SemanticSupportsExpressionSigil<SemanticEntitySelector>,
        SemanticExpression,
    ),
    Return(SemanticReturnCommand),
    Scoreboard(SemanticScoreboardCommand),
    Stopwatch(SemanticStopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<SemanticExpression>,
    ),
}

impl SemanticCommand {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Command {
        match self {
            Self::Data(data_command) => Command::Data(data_command.compile(datapack, ctx)),
            Self::Difficulty(difficulty) => Command::Difficulty(difficulty),
            Self::Enchant(selector, location, level) => {
                Command::Enchant(selector.compile(datapack, ctx), location, level)
            }
            Self::Execute(execute_subcommand) => {
                Command::Execute(execute_subcommand.compile(datapack, ctx))
            }
            Self::Function(id, arguments) => {
                let id = id.compile(datapack, ctx);

                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(datapack, ctx));

                Command::Function(id, compiled_arguments)
            }
            Self::Tellraw(selector, expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                Command::Tellraw(
                    selector.compile(datapack, ctx),
                    expression.as_text_component(datapack, ctx, false),
                )
            }
            Self::Return(command) => match command {
                SemanticReturnCommand::Fail | SemanticReturnCommand::Value(0) => {
                    Command::Return(ReturnCommand::Fail)
                }
                SemanticReturnCommand::Value(value) => Command::Return(ReturnCommand::Value(value)),
                SemanticReturnCommand::Run(command) => {
                    Command::Return(ReturnCommand::Run(Box::new(command.compile(datapack, ctx))))
                }
            },
            Self::Scoreboard(command) => Command::Scoreboard(command.compile(datapack, ctx)),
            Self::Stopwatch(command) => {
                let command = command.compile(datapack, ctx);

                Command::Stopwatch(command)
            }
            Self::Summon(entity, position, nbt) => Command::Summon(
                entity,
                position,
                nbt.map(|nbt| nbt.kind.resolve(datapack, ctx).as_snbt_macros(ctx)),
            ),
        }
    }
}
