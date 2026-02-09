use std::collections::BTreeMap;

use kelp_core::{
    data_type::high::{HighDataType, HighDataTypeKind},
    high::snbt_string::HighSNBTString,
};
use minecraft_command_types::snbt::SNBTString;
use parser_rs::{
    combinators::{char, choice::choice},
    fn_parser::FnParser,
    parser_range::ParserRange,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

use crate::{identifier, inline_whitespace, whitespace};

pub fn parse_typed_compound(input: &mut Stream) -> Option<HighDataType> {
    let start = input.position;

    char('{').parse(input)?;
    whitespace(input)?;
    let elements: Vec<(HighSNBTString, HighDataType)> = (|input: &mut Stream| {
        whitespace(input)?;
        let (key_span, key) = identifier("field name")
            .spanned()
            .syntax(SemanticTokenKind::Variable)
            .parse(input)?;
        whitespace(input)?;
        char(':').parse(input)?;
        whitespace(input)?;
        let value = parse_data_type(input)?;
        whitespace(input)?;

        Some((
            HighSNBTString {
                span: key_span,
                snbt_string: SNBTString(false, key.to_string()),
            },
            value,
        ))
    })
    .separated_by(char(','))
    .parse(input)?;
    char('}').parse(input)?;
    Some(HighDataType {
        span: ParserRange {
            start,
            end: input.position,
        },
        kind: HighDataTypeKind::TypedCompound(elements.into_iter().collect::<BTreeMap<_, _>>()),
    })
}

pub fn parse_data_type(input: &mut Stream) -> Option<HighDataType> {
    choice((
        |input: &mut Stream| {
            let start = input.position;

            char('&').parse(input)?;
            let inner = parse_data_type(input)?;
            Some(HighDataType {
                span: ParserRange {
                    start,
                    end: input.position,
                },
                kind: HighDataTypeKind::Reference(Box::new(inner)),
            })
        },
        |input: &mut Stream| {
            let start = input.position;

            char('(').parse(input)?;
            whitespace(input)?;
            char(')').parse(input)?;

            Some(HighDataType {
                span: ParserRange {
                    start,
                    end: input.position,
                },
                kind: HighDataTypeKind::Unit,
            })
        },
        parse_typed_compound,
        |input: &mut Stream| {
            let start = input.position;

            let (name_span, name) = identifier("data type")
                .spanned()
                .syntax(SemanticTokenKind::Class)
                .parse(input)?;

            let generics = (|input: &mut Stream| {
                char('<').parse(input)?;
                inline_whitespace(input)?;

                let args = parse_data_type
                    .separated_by(|input: &mut Stream| {
                        inline_whitespace(input)?;
                        char(',').parse(input)?;
                        inline_whitespace(input)?;
                        Some(())
                    })
                    .parse(input)?;

                inline_whitespace(input)?;
                char('>').parse(input)?;
                Some(args)
            })
            .optional()
            .parse(input)?
            .unwrap_or_default();

            Some(HighDataType {
                span: ParserRange {
                    start,
                    end: input.position,
                },
                kind: HighDataTypeKind::Named(name_span, name.to_string(), generics),
            })
        },
    ))
    .parse(input)
}
