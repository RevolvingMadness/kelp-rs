use minecraft_command_types::command::{
    ScoreValue,
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand as LowPlayersDisplayScoreboardCommand,
        PlayersScoreboardCommand as LowPlayersScoreboardCommand,
        ScoreboardNumberFormat as LowScoreboardNumberFormat,
    },
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    low::{
        entity_selector::EntitySelector,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionId},
        player_score::PlayerScore,
    },
};

#[derive(Debug, Clone, Copy)]
pub enum ScoreboardNumberFormat {
    Blank,
    Fixed(UnresolvedExpressionId),
    Styled(UnresolvedExpressionId),
}

impl ScoreboardNumberFormat {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardNumberFormat {
        match self {
            Self::Blank => LowScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => LowScoreboardNumberFormat::Fixed(
                UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx),
            ),
            Self::Styled(expression) => LowScoreboardNumberFormat::Styled(
                UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<UnresolvedExpressionId>),
    NumberFormat(PlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowPlayersDisplayScoreboardCommand {
        match self {
            Self::Name(score, expression) => {
                let score = score.compile(allocator, datapack, ctx).score;
                let snbt = expression.map(|expression| {
                    UnresolvedExpression::resolve(expression, allocator, datapack, ctx)
                        .as_snbt_macros(ctx)
                });

                LowPlayersDisplayScoreboardCommand::Name(score, snbt)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.compile(allocator, datapack, ctx).score;
                let number_format = number_format
                    .map(|number_format| number_format.compile(allocator, datapack, ctx));

                LowPlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PlayersScoreboardCommand {
    List(Option<EntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, ScoreValue),
    Add(PlayerScore, ScoreValue),
    Remove(PlayerScore, ScoreValue),
    Reset(EntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowPlayersScoreboardCommand {
        match self {
            Self::List(selector) => {
                let selector = selector.map(|selector| selector.compile(allocator, datapack, ctx));

                LowPlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.compile(allocator, datapack, ctx);

                LowPlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.compile(allocator, datapack, ctx).score;
                let right = right.compile(allocator, datapack, ctx).score;

                LowPlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.compile(allocator, datapack, ctx);

                LowPlayersScoreboardCommand::Display(command)
            }
        }
    }
}
