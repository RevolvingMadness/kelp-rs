use crate::command::HighCommand;
use crate::enums::parse_difficulty;
use crate::required_inline_whitespace;
use parser_rs::{combinators::suggest_literal, fn_parser::FnParser, stream::Stream};

pub fn parse_difficulty_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("difficulty")
            .syntax_keyword()
            .parse(input)?;
        let difficulty = (|input: &mut Stream| {
            required_inline_whitespace.parse(input)?;
            parse_difficulty.next_signature_parameter().parse(input)
        })
        .optional()
        .parse(input)?;

        Some(HighCommand::Difficulty(difficulty))
    })
    .signature(0)
    .signatures(&[(
        "difficulty <difficulty>",
        &[("<difficulty>", Some("The difficulty to set to"))],
        Some("Changes the difficulty to <difficulty>"),
    )])
    .parse(input)
}
