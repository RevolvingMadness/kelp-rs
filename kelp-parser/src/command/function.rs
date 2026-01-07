use crate::command::HighCommand;
use crate::expression::expression;
use crate::required_inline_whitespace;
use crate::resource_location::parse_resource_location;
use kelp_core::command::function::HighFunctionCommandArguments;
use kelp_core::expression::ExpressionKind;
use parser_rs::{combinators::suggest_literal, fn_parser::FnParser, stream::Stream};

pub fn parse_function_command(input: &mut Stream) -> Option<HighCommand> {
    (|input: &mut Stream| {
        suggest_literal("function").syntax_keyword().parse(input)?;
        required_inline_whitespace.parse(input)?;
        let id = parse_resource_location
            .next_signature_parameter()
            .parse(input)?;
        let arguments: Option<Option<HighFunctionCommandArguments>> = (|input: &mut Stream| {
            required_inline_whitespace.parse(input)?;

            expression
                .next_signature_parameter()
                .spanned()
                .map_input(|input, (span, value)| {
                    // TODO make this regular expression?
                    if let ExpressionKind::Compound(compound) = value.kind {
                        Some(HighFunctionCommandArguments::Compound(compound))
                    } else {
                        input.add_validation_error_span(
                            span,
                            "Function arguments must be a compound",
                        );

                        None
                    }
                })
                .parse(input)
        })
        .optional()
        .parse(input)?;

        Some(HighCommand::Function(id, arguments.flatten()))
    })
    .signature(0)
    .signatures(&[(
        "function <function> [<arguments>]",
        &[
            ("<function>", Some("The id of the function to run")),
            ("[<value>]", Some("The value to send")),
        ],
        Some("Runs <function> with optional [<value>]"),
    )])
    .parse(input)
}
