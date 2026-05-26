use minecraft_command_types::command::{
    ScoreValue,
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand, PlayersScoreboardCommand, ScoreboardNumberFormat,
    },
};

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::expression::SemanticExpression,
    semantic::{entity_selector::SemanticEntitySelector, player_score::SemanticPlayerScore},
};

#[derive(Debug, Clone)]
pub enum SemanticScoreboardNumberFormat {
    Blank,
    Fixed(SemanticExpression),
    Styled(SemanticExpression),
}

impl SemanticScoreboardNumberFormat {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardNumberFormat {
        match self {
            Self::Blank => ScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => ScoreboardNumberFormat::Fixed(
                expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
            Self::Styled(expression) => ScoreboardNumberFormat::Styled(
                expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticPlayersDisplayScoreboardCommand {
    Name(SemanticPlayerScore, Option<SemanticExpression>),
    NumberFormat(SemanticPlayerScore, Option<SemanticScoreboardNumberFormat>),
}

impl SemanticPlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> PlayersDisplayScoreboardCommand {
        match self {
            Self::Name(score, expression) => {
                let score = score.compile(datapack, ctx).score;
                let snbt = expression
                    .map(|expression| expression.kind.resolve(datapack, ctx).as_snbt_macros(ctx));

                PlayersDisplayScoreboardCommand::Name(score, snbt)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.compile(datapack, ctx).score;
                let number_format =
                    number_format.map(|number_format| number_format.compile(datapack, ctx));

                PlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticPlayersScoreboardCommand {
    List(Option<SemanticEntitySelector>),
    Get(SemanticPlayerScore),
    Set(SemanticPlayerScore, ScoreValue),
    Add(SemanticPlayerScore, ScoreValue),
    Remove(SemanticPlayerScore, ScoreValue),
    Reset(SemanticEntitySelector, Option<String>),
    Enable(SemanticPlayerScore),
    Operation(
        SemanticPlayerScore,
        ScoreOperationOperator,
        SemanticPlayerScore,
    ),
    Display(Box<SemanticPlayersDisplayScoreboardCommand>),
}

impl SemanticPlayersScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> PlayersScoreboardCommand {
        match self {
            Self::List(selector) => {
                let selector = selector.map(|selector| selector.compile(datapack, ctx));

                PlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.compile(datapack, ctx);

                PlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.compile(datapack, ctx).score;
                let right = right.compile(datapack, ctx).score;

                PlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.compile(datapack, ctx);

                PlayersScoreboardCommand::Display(command)
            }
        }
    }
}
