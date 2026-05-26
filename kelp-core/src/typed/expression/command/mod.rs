use minecraft_command_types::{
    command::{Command, enums::difficulty::Difficulty, r#return::ReturnCommand},
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::{
        entity_selector::TypedEntitySelector,
        expression::{
            TypedExpression, TypedExpressionId,
            command::{
                data::TypedDataCommand, execute::subcommand::TypedExecuteSubcommand,
                function::TypedFunctionCommandArguments, r#return::TypedReturnCommand,
                scoreboard::TypedScoreboardCommand, stopwatch::TypedStopwatchCommand,
            },
        },
        supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

pub mod data;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod stopwatch;

#[derive(Debug, Clone)]
pub enum TypedCommand {
    Data(TypedDataCommand),
    Difficulty(Option<Difficulty>),
    Enchant(
        TypedSupportsExpressionSigil<TypedEntitySelector>,
        ResourceLocation,
        Option<i32>,
    ),
    Execute(TypedExecuteSubcommand),
    Function(
        TypedSupportsExpressionSigil<ResourceLocation>,
        Option<TypedFunctionCommandArguments>,
    ),
    Tellraw(
        TypedSupportsExpressionSigil<TypedEntitySelector>,
        TypedExpressionId,
    ),
    Return(TypedReturnCommand),
    Scoreboard(TypedScoreboardCommand),
    Stopwatch(TypedStopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<TypedExpressionId>,
    ),
}

impl TypedCommand {
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Command {
        match self {
            Self::Data(data_command) => Command::Data(data_command.compile(arena, datapack, ctx)),
            Self::Difficulty(difficulty) => Command::Difficulty(difficulty),
            Self::Enchant(selector, location, level) => {
                Command::Enchant(selector.compile(arena, datapack, ctx), location, level)
            }
            Self::Execute(execute_subcommand) => {
                Command::Execute(execute_subcommand.compile(arena, datapack, ctx))
            }
            Self::Function(id, arguments) => {
                let id = id.compile(arena, datapack, ctx);

                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(arena, datapack, ctx));

                Command::Function(id, compiled_arguments)
            }
            Self::Tellraw(selector, expression) => {
                let expression = TypedExpression::resolve(expression, arena, datapack, ctx);

                Command::Tellraw(
                    selector.compile(arena, datapack, ctx),
                    expression.as_text_component(datapack, ctx, false),
                )
            }
            Self::Return(command) => match command {
                TypedReturnCommand::Fail | TypedReturnCommand::Value(0) => {
                    Command::Return(ReturnCommand::Fail)
                }
                TypedReturnCommand::Value(value) => Command::Return(ReturnCommand::Value(value)),
                TypedReturnCommand::Run(command) => Command::Return(ReturnCommand::Run(Box::new(
                    command.compile(arena, datapack, ctx),
                ))),
            },
            Self::Scoreboard(command) => Command::Scoreboard(command.compile(arena, datapack, ctx)),
            Self::Stopwatch(command) => {
                let command = command.compile(arena, datapack, ctx);

                Command::Stopwatch(command)
            }
            Self::Summon(entity, position, nbt) => Command::Summon(
                entity,
                position,
                nbt.map(|nbt| {
                    let nbt = TypedExpression::resolve(nbt, arena, datapack, ctx);

                    nbt.as_snbt_macros(ctx)
                }),
            ),
        }
    }
}
