use crate::command::HighCommand;
use crate::entity_selector::parse_entity_selector;
use crate::resource_location::parse_resource_location;
use crate::{integer, required_inline_whitespace};
use parser_rs::{FnParser, Stream, suggest_literal};

pub fn parse_enchant_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("enchant").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;
        let selector = parse_entity_selector
            .next_signature_parameter()
            .parse(input)?;
        required_inline_whitespace.parse(input)?;
        let location = parse_resource_location
            .next_signature_parameter()
            .parse(input)?;
        let level = (|input: &mut Stream| {
            required_inline_whitespace.parse(input)?;
            integer.next_signature_parameter().parse(input)
        })
        .optional()
        .parse(input)?;

        Some(HighCommand::Enchant(selector, location, level))
    })
    .signature(0)
    .signatures(&[(
        "enchant <selector> <enchantment> [<level>]",
        &[
            ("<selector>", Some("The entity to target")),
            ("<enchantment>", Some("The enchantment to enchant with")),
            ("[<level>]", Some("The level of <enchantment>")),
        ],
        Some("Enchants <selector> to <enchantment> (optionally with [<level>])"),
    )])
    .parse(input)
}
