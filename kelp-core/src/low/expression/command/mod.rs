use la_arena::Idx;
use minecraft_command_types::{
    command::{
        Command as LowCommand, enums::difficulty::Difficulty,
        r#return::ReturnCommand as LowReturnCommand,
    },
    coordinate::Coordinates,
    resource_location::ResourceLocation,
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        entity_selector::EntitySelector,
        expression::{
            command::{
                data::DataCommand, execute::subcommand::ExecuteSubcommand,
                function::FunctionCommandArguments, r#return::ReturnCommand,
                scoreboard::ScoreboardCommand, stopwatch::StopwatchCommand,
            },
            unresolved::UnresolvedExpression,
        },
        supports_expression_sigil::SupportsExpressionSigil,
    },
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
    Enchant(
        SupportsExpressionSigil<EntitySelector>,
        ResourceLocation,
        Option<i32>,
    ),
    Execute(ExecuteSubcommand),
    Function(
        SupportsExpressionSigil<ResourceLocation>,
        Option<FunctionCommandArguments>,
    ),
    Tellraw(
        SupportsExpressionSigil<EntitySelector>,
        Idx<UnresolvedExpression>,
    ),
    Return(ReturnCommand),
    Scoreboard(ScoreboardCommand),
    Stopwatch(StopwatchCommand),
    Summon(
        ResourceLocation,
        Option<Coordinates>,
        Option<Idx<UnresolvedExpression>>,
    ),
}

impl Command {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowCommand {
        match self {
            Self::Data(data_command) => {
                LowCommand::Data(data_command.compile(allocator, datapack, ctx))
            }
            Self::Difficulty(difficulty) => LowCommand::Difficulty(difficulty),
            Self::Enchant(selector, location, level) => {
                LowCommand::Enchant(selector.compile(allocator, datapack, ctx), location, level)
            }
            Self::Execute(execute_subcommand) => {
                LowCommand::Execute(execute_subcommand.compile(allocator, datapack, ctx))
            }
            Self::Function(id, arguments) => {
                let id = id.compile(allocator, datapack, ctx);

                let compiled_arguments =
                    arguments.map(|arguments| arguments.compile(allocator, datapack, ctx));

                LowCommand::Function(id, compiled_arguments)
            }
            Self::Tellraw(selector, expression) => {
                let expression =
                    UnresolvedExpression::resolve(expression, allocator, datapack, ctx);

                LowCommand::Tellraw(
                    selector.compile(allocator, datapack, ctx),
                    expression.as_text_component(datapack, ctx, false),
                )
            }
            Self::Return(command) => match command {
                ReturnCommand::Fail | ReturnCommand::Value(0) => {
                    LowCommand::Return(LowReturnCommand::Fail)
                }
                ReturnCommand::Value(value) => LowCommand::Return(LowReturnCommand::Value(value)),
                ReturnCommand::Run(command) => LowCommand::Return(LowReturnCommand::Run(Box::new(
                    command.compile(allocator, datapack, ctx),
                ))),
            },
            Self::Scoreboard(command) => {
                LowCommand::Scoreboard(command.compile(allocator, datapack, ctx))
            }
            Self::Stopwatch(command) => {
                let command = command.compile(allocator, datapack, ctx);

                LowCommand::Stopwatch(command)
            }
            Self::Summon(entity, position, nbt) => LowCommand::Summon(
                entity,
                position,
                nbt.map(|nbt| {
                    let nbt = UnresolvedExpression::resolve(nbt, allocator, datapack, ctx);

                    nbt.as_snbt_macros(ctx)
                }),
            ),
        }
    }
}
