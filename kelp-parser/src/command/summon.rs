use crate::command::{HighCommand, split_2};
use crate::coordinate::parse_coordinates;
use crate::expression::expression;
use crate::required_inline_whitespace;
use crate::resource_location::parse_resource_location;
use parser_rs::Stream;
use parser_rs::{FnParser, suggest_literal};

pub fn parse_summon_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("summon").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;
        let entity = parse_resource_location
            .next_signature_parameter()
            .parse(input)?;
        let (position, nbt) = split_2(
            (|input: &mut Stream| {
                required_inline_whitespace.parse(input)?;
                let position = parse_coordinates.next_signature_parameter().parse(input)?;
                let nbt = (|input: &mut Stream| {
                    required_inline_whitespace.parse(input)?;
                    expression.next_signature_parameter().parse(input)
                })
                .optional()
                .parse(input)?;

                Some((position, nbt))
            })
            .optional()
            .parse(input)?,
        );

        Some(HighCommand::Summon(entity, position, nbt))
    })
    .signature(0)
    .signatures(&[(
        "summon <entity> [<pos>] [<nbt>]",
        &[
            ("<entity>", Some("The entity to summon")),
            ("[<pos>]", Some("The position to summon the entity")),
            ("[<nbt>]", Some("Optional nbt to summon the entity with")),
        ],
        Some("Summons <entity> (optionally at [<pos>]) (optionally with [<nbt>])"),
    )])
    .parse(input)
}
