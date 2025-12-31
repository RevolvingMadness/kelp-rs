use crate::command::HighCommand;
use crate::entity_selector::parse_entity_selector;
use crate::expression::expression;
use crate::required_inline_whitespace;
use parser_rs::FnParser;
use parser_rs::{Stream, suggest_literal};

pub fn parse_tellraw_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("tellraw").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;
        let selector = parse_entity_selector
            .next_signature_parameter()
            .parse(input)?;
        required_inline_whitespace.parse(input)?;
        let value = expression.next_signature_parameter().parse(input)?;

        Some(HighCommand::Tellraw(selector, value))
    })
    .signature(0)
    .signatures(&[(
        "tellraw <selector> <message>",
        &[
            ("<selector>", Some("Who <message> will receive")),
            ("<message>", Some("The message to send")),
        ],
        Some("Sends <message> to <selector>"),
    )])
    .parse(input)
}
