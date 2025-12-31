use crate::command::{HighCommand, parse_command};
use crate::{integer, required_inline_whitespace};
use kelp_core::command::r#return::HighReturnCommand;
use parser_rs::{FnParser, choice};
use parser_rs::{Stream, suggest_literal};

pub fn parse_return_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("return").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;

        let command = choice((
            integer
                .next_signature_parameter()
                .signature(0)
                .map(HighReturnCommand::Value),
            suggest_literal("fail")
                .signature(1)
                .map_to(HighReturnCommand::Fail),
            (|input: &mut Stream| {
                (|input: &mut Stream| {
                    suggest_literal("run").parse(input)?;
                    required_inline_whitespace(input)
                })
                .optional()
                .parse(input)?;

                let command = parse_command.next_signature_parameter().parse(input)?;

                Some(HighReturnCommand::Run(Box::new(command)))
            })
            .signature(2),
        ))
        .signatures(&[
            (
                "<value>",
                &[("<value>", Some("The value to return"))],
                Some("Returns <value> from the current function"),
            ),
            (
                "fail",
                &[],
                Some("Returns 0 (fails) from the current function"),
            ),
            (
                "run <command>",
                &[("<command>", Some("The command to return"))],
                Some("Runs <command> and returns the result from the current function"),
            ),
        ])
        .next_signature_parameter()
        .parse(input)?;

        Some(HighCommand::Return(command))
    })
    .signature(0)
    .signatures(&[(
        "return <value>",
        &[("<value>", Some("The value to return"))],
        Some("Returns <value> from the current function"),
    )])
    .parse(input)
}
