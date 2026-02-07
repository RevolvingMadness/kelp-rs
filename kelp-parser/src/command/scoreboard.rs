use crate::command::HighCommand;
use crate::entity_selector::parse_entity_selector;
use crate::expression::expression;
use crate::{boolean, identifier, integer, key, required_inline_whitespace};
use kelp_core::high::command::scoreboard::HighScoreboardCommand;
use kelp_core::high::command::scoreboard::objectives::{
    HighObjectivesScoreboardCommand, HighScoreboardModification,
};
use kelp_core::high::command::scoreboard::players::{
    HighPlayersDisplayScoreboardCommand, HighPlayersScoreboardCommand, HighScoreboardNumberFormat,
};
use kelp_core::high::player_score::HighPlayerScore;
use minecraft_command_types::command::enums::score_operation_operator::ScoreOperationOperator;
use parser_rs::{
    combinators::{char, choice::choice, literal, suggest_literal},
    fn_parser::FnParser,
    stream::Stream,
};

fn parse_number_format(input: &mut Stream) -> Option<HighScoreboardNumberFormat> {
    choice((
        (|input: &mut Stream| {
            suggest_literal("blank").parse(input)?;
            Some(HighScoreboardNumberFormat::Blank)
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("fixed").parse(input)?;
            required_inline_whitespace(input)?;
            let value = expression.next_signature_parameter().parse(input)?;
            Some(HighScoreboardNumberFormat::Fixed(value))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("styled").parse(input)?;
            required_inline_whitespace(input)?;
            let value = expression.next_signature_parameter().parse(input)?;
            Some(HighScoreboardNumberFormat::Styled(value))
        })
        .signature(2),
    ))
    .signatures(&[
        (
            "blank",
            &[],
            Some("Changes the scoreboard's number format to blank"),
        ),
        (
            "fixed <value>",
            &[(
                "<value>",
                Some("The value to use for the scoreboard's number format"),
            )],
            Some("Makes the scoreboard's number format fixed"),
        ),
        (
            "styled <value>",
            &[(
                "<value>",
                Some("The value to use for the scoreboard's number format"),
            )],
            Some("Changes the scoreboard's number format style to <value>"),
        ),
    ])
    .parse(input)
}

fn parse_player_score(input: &mut Stream) -> Option<HighPlayerScore> {
    (|input: &mut Stream| {
        let targets = parse_entity_selector
            .next_signature_parameter()
            .parse(input)?;
        required_inline_whitespace(input)?;
        let objective = parse_objective.next_signature_parameter().parse(input)?;

        Some(HighPlayerScore::new(targets, objective.to_string()))
    })
    .signature(0)
    .signatures(&[(
        "<targets> <objective>",
        &[
            ("<targets>", Some("The targets of the objective")),
            ("<objective>", Some("The objective")),
        ],
        None,
    )])
    .parse(input)
}

fn parse_objective<'a>(input: &mut Stream<'a>) -> Option<&'a str> {
    identifier("objective name").parse(input)
}

fn parse_objectives(input: &mut Stream) -> Option<HighObjectivesScoreboardCommand> {
    choice((
        (|input: &mut Stream| {
            suggest_literal("list").parse(input)?;

            Some(HighObjectivesScoreboardCommand::List)
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("add").parse(input)?;
            required_inline_whitespace(input)?;
            let name = parse_objective.next_signature_parameter().parse(input)?;
            required_inline_whitespace(input)?;
            let criterion = parse_objective.next_signature_parameter().parse(input)?;
            let display = (|input: &mut Stream| {
                required_inline_whitespace(input)?;
                expression.next_signature_parameter().parse(input)
            })
            .optional()
            .parse(input)?;

            Some(HighObjectivesScoreboardCommand::Add(
                name.to_string(),
                criterion.to_string(),
                display,
            ))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("remove").parse(input)?;
            required_inline_whitespace(input)?;
            let name = identifier("objective name")
                .next_signature_parameter()
                .parse(input)?;

            Some(HighObjectivesScoreboardCommand::Remove(name.to_string()))
        })
        .signature(2),
        (|input: &mut Stream| {
            suggest_literal("setdisplay").parse(input)?;
            required_inline_whitespace(input)?;
            let slot = key("scoreboard display slot")
                .next_signature_parameter()
                .parse(input)?;
            let objective = (|input: &mut Stream| {
                required_inline_whitespace(input)?;
                identifier("objective name")
                    .map(ToString::to_string)
                    .next_signature_parameter()
                    .parse(input)
            })
            .optional()
            .parse(input)?;

            Some(HighObjectivesScoreboardCommand::SetDisplay(
                slot.to_string(),
                objective,
            ))
        })
        .signature(3),
        (|input: &mut Stream| {
            suggest_literal("modify").parse(input)?;
            required_inline_whitespace(input)?;
            let name = identifier("objective name")
                .next_signature_parameter()
                .parse(input)?;
            required_inline_whitespace(input)?;
            let modification = choice((
                (|input: &mut Stream| {
                    suggest_literal("displayautoupdate").parse(input)?;
                    required_inline_whitespace(input)?;
                    let value = boolean.next_signature_parameter().parse(input)?;

                    Some(HighScoreboardModification::DisplayAutoUpdate(value))
                })
                .signature(0),
                (|input: &mut Stream| {
                    suggest_literal("displayname").parse(input)?;
                    required_inline_whitespace(input)?;
                    let value = expression.next_signature_parameter().parse(input)?;

                    Some(HighScoreboardModification::DisplayName(value))
                })
                .signature(1),
                (|input: &mut Stream| {
                    suggest_literal("numberformat").parse(input)?;
                    let number_format = (|input: &mut Stream| {
                        required_inline_whitespace(input)?;
                        parse_number_format.next_signature_parameter().parse(input)
                    })
                    .optional()
                    .parse(input)?;

                    Some(HighScoreboardModification::NumberFormat(number_format))
                })
                .signature(2),
            ))
            .signatures(&[
                (
                    "displayautoupdate <value>",
                    &[("<value>", Some("Should the display auto update"))],
                    Some("Changes whether the scoreboard's display should automatically update"),
                ),
                (
                    "displayname <value>",
                    &[(
                        "<value>",
                        Some("The text component of the scoreboard's display"),
                    )],
                    Some("Changes how the scoreboard looks"),
                ),
                (
                    "numberformat [...]",
                    &[("[...]", None)],
                    Some("Changes the scoreboard's number format"),
                ),
            ])
            .next_signature_parameter()
            .parse(input)?;

            Some(HighObjectivesScoreboardCommand::Modify(
                name.to_string(),
                modification,
            ))
        })
        .signature(4),
    ))
    .signatures(&[
        ("list", &[], Some("Lists all scoreboard objectives")),
        (
            "add <name> <criterion> [<display>]",
            &[
                ("<name>", Some("The name of the scoreboard objective")),
                ("<criterion>", Some("How the scoreboard will be counted")),
                (
                    "[<display>]",
                    Some("How the title of the scoreboard will be displayed"),
                ),
            ],
            Some("Creates a new scoreboard objective"),
        ),
        (
            "remove <name>",
            &[("<name>", Some("The name of the objective to remove"))],
            Some("Removes a scoreboard objectives"),
        ),
        (
            "setdisplay <slot> [<objective>]",
            &[
                ("<slot>", Some("Where to display the scoreboard")),
                ("[<objective>]", Some("The name of the objective to target")),
            ],
            Some("Clears or displays the given objective to slot>"),
        ),
        (
            "modify <objective> ...",
            &[("<objective>", Some("The objective to modify"))],
            Some("Modifies an attribute of <objective>"),
        ),
    ])
    .parse(input)
}

fn parse_players(input: &mut Stream) -> Option<HighPlayersScoreboardCommand> {
    choice((
        (|input: &mut Stream| {
            suggest_literal("add").parse(input)?;
            required_inline_whitespace(input)?;
            let player_score = parse_player_score.next_signature_parameter().parse(input)?;
            required_inline_whitespace(input)?;
            let amount = integer.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Add(player_score, amount))
        })
        .signature(0),
        (|input: &mut Stream| {
            suggest_literal("display").parse(input)?;
            required_inline_whitespace(input)?;
            let command = choice((
                (|input: &mut Stream| {
                    suggest_literal("name").parse(input)?;
                    required_inline_whitespace(input)?;
                    let player_score =
                        parse_player_score.next_signature_parameter().parse(input)?;
                    let name = (|input: &mut Stream| {
                        required_inline_whitespace(input)?;
                        expression.next_signature_parameter().parse(input)
                    })
                    .optional()
                    .parse(input)?;

                    Some(HighPlayersDisplayScoreboardCommand::Name(
                        player_score,
                        name,
                    ))
                })
                .signature(0),
                (|input: &mut Stream| {
                    suggest_literal("numberformat").parse(input)?;
                    required_inline_whitespace(input)?;
                    let player_score =
                        parse_player_score.next_signature_parameter().parse(input)?;
                    let number_format = (|input: &mut Stream| {
                        required_inline_whitespace(input)?;
                        parse_number_format.next_signature_parameter().parse(input)
                    })
                    .optional()
                    .parse(input)?;

                    Some(HighPlayersDisplayScoreboardCommand::NumberFormat(
                        player_score,
                        number_format,
                    ))
                })
                .signature(1),
            ))
            .signatures(&[
                (
                    "name <score> <name>",
                    &[
                        ("<score>", Some("The score to perform the action on")),
                        ("<name>", Some("The name to display")),
                    ],
                    Some("Changes the name of the scoreboard"),
                ),
                (
                    "numberformat <score> ...",
                    &[
                        ("<score>", Some("The score to perform the action on")),
                        ("...", None),
                    ],
                    Some("Changes the number format of the scoreboard"),
                ),
            ])
            .parse(input)?;

            Some(HighPlayersScoreboardCommand::Display(Box::new(command)))
        })
        .signature(1),
        (|input: &mut Stream| {
            suggest_literal("enable").parse(input)?;
            required_inline_whitespace(input)?;
            let player_score = parse_player_score.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Enable(player_score))
        })
        .signature(2),
        (|input: &mut Stream| {
            suggest_literal("get").parse(input)?;
            required_inline_whitespace(input)?;
            let player_score = parse_player_score.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Get(player_score))
        })
        .signature(3),
        (|input: &mut Stream| {
            suggest_literal("list").parse(input)?;
            let targets = (|input: &mut Stream| {
                required_inline_whitespace(input)?;
                parse_entity_selector
                    .next_signature_parameter()
                    .parse(input)
            })
            .optional()
            .parse(input)?;

            Some(HighPlayersScoreboardCommand::List(targets))
        })
        .signature(4),
        (|input: &mut Stream| {
            suggest_literal("operation").parse(input)?;
            required_inline_whitespace(input)?;
            let left = parse_player_score.next_signature_parameter().parse(input)?;
            required_inline_whitespace(input)?;
            let operator = choice((
                char('=').map_to(ScoreOperationOperator::Set),
                literal("+=").map_to(ScoreOperationOperator::Add),
                literal("-=").map_to(ScoreOperationOperator::Subtract),
                literal("*=").map_to(ScoreOperationOperator::Multiply),
                literal("/=").map_to(ScoreOperationOperator::Divide),
                literal("%=").map_to(ScoreOperationOperator::Modulo),
                literal("><").map_to(ScoreOperationOperator::Swap),
                literal("<").map_to(ScoreOperationOperator::ChooseMinimum),
                literal(">").map_to(ScoreOperationOperator::ChooseMaximum),
            ))
            .next_signature_parameter()
            .parse(input)?;
            required_inline_whitespace(input)?;
            let right = parse_player_score.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Operation(
                left, operator, right,
            ))
        })
        .signature(5),
        (|input: &mut Stream| {
            suggest_literal("remove").parse(input)?;
            required_inline_whitespace(input)?;
            let player_score = parse_player_score.next_signature_parameter().parse(input)?;
            required_inline_whitespace(input)?;
            let amount = integer.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Remove(player_score, amount))
        })
        .signature(6),
        (|input: &mut Stream| {
            suggest_literal("reset").parse(input)?;
            required_inline_whitespace(input)?;
            let targets = parse_entity_selector
                .next_signature_parameter()
                .parse(input)?;
            let objective = (|input: &mut Stream| {
                required_inline_whitespace(input)?;
                let result = parse_objective.next_signature_parameter().parse(input)?;

                Some(result.to_string())
            })
            .optional()
            .parse(input)?;

            Some(HighPlayersScoreboardCommand::Reset(targets, objective))
        })
        .signature(7),
        (|input: &mut Stream| {
            suggest_literal("set").parse(input)?;
            required_inline_whitespace(input)?;
            let player_score = parse_player_score.next_signature_parameter().parse(input)?;
            required_inline_whitespace(input)?;
            let amount = integer.next_signature_parameter().parse(input)?;

            Some(HighPlayersScoreboardCommand::Set(player_score, amount))
        })
        .signature(8),
    ))
    .signatures(&[
        (
            "add <score> <amount>",
            &[
                ("<score>", Some("The score to perform the action on")),
                ("<amount>", Some("The amount to add to the score")),
            ],
            Some("Adds <amount> to the score of <score>"),
        ),
        (
            "display ...",
            &[("...", None)],
            Some("Changes how the <targets> score is displayed"),
        ),
        (
            "enable <score>",
            &[("<score>", Some("The score to enable"))],
            Some("Enables the scoreboard for the given score"),
        ),
        (
            "get <score>",
            &[("<score>", Some("The score to get"))],
            Some("Gets the score of <score>"),
        ),
        (
            "list [<targets>]",
            &[("<targets>", Some("The targets to list the scores of"))],
            Some("Lists the scores of <targets>"),
        ),
        (
            "operation <left> <operator> <right>",
            &[
                ("<left>", Some("The left operand")),
                ("<operator>", Some("The operator to use")),
                ("<right>", Some("The right operand")),
            ],
            Some("Performs arithmetic on the scores of <left> and <right>"),
        ),
        (
            "remove <score> <amount>",
            &[
                ("<score>", Some("The score to remove the score of")),
                ("<amount>", Some("The amount to remove from the score")),
            ],
            Some("Removes <amount> from the score of <targets> in <objective>"),
        ),
        (
            "reset <targets> [<objective>]",
            &[
                ("<targets>", Some("The targets to reset the scores of")),
                (
                    "[<objective>]",
                    Some("The objective to reset the scores of"),
                ),
            ],
            Some("Resets the scores of <targets> in <objective>"),
        ),
        (
            "set <score> <value>",
            &[
                ("<score>", Some("The score to set the value of")),
                ("<value>", Some("The value to set the score to")),
            ],
            Some("Sets the score of <score> to <value>"),
        ),
    ])
    .parse(input)
}

pub fn parse_scoreboard_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("scoreboard")
            .syntax_keyword()
            .parse(input)?;
        required_inline_whitespace.parse(input)?;

        let command = choice((
            (|input: &mut Stream| {
                suggest_literal("objectives").parse(input)?;
                required_inline_whitespace(input)?;
                let result = parse_objectives.next_signature_parameter().parse(input)?;

                Some(HighScoreboardCommand::Objectives(Box::new(result)))
            })
            .signature(0),
            (|input: &mut Stream| {
                suggest_literal("players").parse(input)?;
                required_inline_whitespace(input)?;
                let result = parse_players.next_signature_parameter().parse(input)?;

                Some(HighScoreboardCommand::Players(result))
            })
            .signature(1),
        ))
        .signatures(&[
            (
                "objectives ...",
                &[("...", None)],
                Some("Performs an operation on an objective"),
            ),
            (
                "players ...",
                &[("...", None)],
                Some("Performs an operation on a player in an objective"),
            ),
        ])
        .next_signature_parameter()
        .parse(input)?;

        Some(HighCommand::Scoreboard(command))
    })
    .signature(0)
    .signatures(&[(
        "scoreboard ...",
        &[("...", None)],
        Some("Performs an action on a scoreboard"),
    )])
    .parse(input)
}
