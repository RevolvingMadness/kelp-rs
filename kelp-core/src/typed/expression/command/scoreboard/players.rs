use minecraft_command_types::command::{
    ScoreValue,
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand, PlayersScoreboardCommand, ScoreboardNumberFormat,
    },
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::{
        entity_selector::TypedEntitySelector,
        expression::typed::{TypedExpression, TypedExpressionId},
        player_score::TypedPlayerScore,
    },
};

#[derive(Debug, Clone, Copy)]
pub enum TypedScoreboardNumberFormat {
    Blank,
    Fixed(TypedExpressionId),
    Styled(TypedExpressionId),
}

impl TypedScoreboardNumberFormat {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardNumberFormat {
        match self {
            Self::Blank => ScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => ScoreboardNumberFormat::Fixed(
                TypedExpression::resolve(expression, allocator, datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::Styled(expression) => ScoreboardNumberFormat::Styled(
                TypedExpression::resolve(expression, allocator, datapack, ctx).as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedPlayersDisplayScoreboardCommand {
    Name(TypedPlayerScore, Option<TypedExpressionId>),
    NumberFormat(TypedPlayerScore, Option<TypedScoreboardNumberFormat>),
}

impl TypedPlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> PlayersDisplayScoreboardCommand {
        match self {
            Self::Name(score, expression) => {
                let score = score.compile(allocator, datapack, ctx).score;
                let snbt = expression.map(|expression| {
                    TypedExpression::resolve(expression, allocator, datapack, ctx)
                        .as_snbt_macros(ctx)
                });

                PlayersDisplayScoreboardCommand::Name(score, snbt)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.compile(allocator, datapack, ctx).score;
                let number_format = number_format
                    .map(|number_format| number_format.compile(allocator, datapack, ctx));

                PlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedPlayersScoreboardCommand {
    List(Option<TypedEntitySelector>),
    Get(TypedPlayerScore),
    Set(TypedPlayerScore, ScoreValue),
    Add(TypedPlayerScore, ScoreValue),
    Remove(TypedPlayerScore, ScoreValue),
    Reset(TypedEntitySelector, Option<String>),
    Enable(TypedPlayerScore),
    Operation(TypedPlayerScore, ScoreOperationOperator, TypedPlayerScore),
    Display(Box<TypedPlayersDisplayScoreboardCommand>),
}

impl TypedPlayersScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> PlayersScoreboardCommand {
        match self {
            Self::List(selector) => {
                let selector = selector.map(|selector| selector.compile(allocator, datapack, ctx));

                PlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.compile(allocator, datapack, ctx);

                PlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.compile(allocator, datapack, ctx).score;
                let right = right.compile(allocator, datapack, ctx).score;

                PlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.compile(allocator, datapack, ctx);

                PlayersScoreboardCommand::Display(command)
            }
        }
    }
}
