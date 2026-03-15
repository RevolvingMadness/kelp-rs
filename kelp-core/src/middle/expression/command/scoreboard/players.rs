use minecraft_command_types::command::{
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand as LowPlayersDisplayScoreboardCommand,
        PlayersScoreboardCommand as LowPlayersScoreboardCommand,
        ScoreboardNumberFormat as LowScoreboardNumberFormat,
    },
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::expression::Expression,
    middle::{entity_selector::EntitySelector, player_score::PlayerScore},
};

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ScoreboardNumberFormat {
    Blank,
    Fixed(Expression),
    Styled(Expression),
}

impl ScoreboardNumberFormat {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardNumberFormat {
        match self {
            Self::Blank => LowScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => LowScoreboardNumberFormat::Fixed(
                expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::Styled(expression) => LowScoreboardNumberFormat::Styled(
                expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum PlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<Expression>),
    NumberFormat(PlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowPlayersDisplayScoreboardCommand {
        match self {
            Self::Name(score, expression) => {
                let score = score.compile(datapack, ctx).score;
                let snbt = expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx));

                LowPlayersDisplayScoreboardCommand::Name(score, snbt)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.compile(datapack, ctx).score;
                let number_format =
                    number_format.map(|number_format| number_format.compile(datapack, ctx));

                LowPlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        }
    }
}

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum PlayersScoreboardCommand {
    List(Option<EntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, i32),
    Add(PlayerScore, i32),
    Remove(PlayerScore, i32),
    Reset(EntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowPlayersScoreboardCommand {
        match self {
            Self::List(selector) => {
                let selector = selector.map(|selector| selector.compile(datapack, ctx));

                LowPlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.compile(datapack, ctx);

                LowPlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.compile(datapack, ctx).score;
                let right = right.compile(datapack, ctx).score;

                LowPlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.compile(datapack, ctx);

                LowPlayersScoreboardCommand::Display(command)
            }
        }
    }
}
