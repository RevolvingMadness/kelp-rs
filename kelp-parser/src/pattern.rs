use std::collections::BTreeMap;

use kelp_core::{
    expression::literal::LiteralExpression,
    high::snbt_string::HighSNBTString,
    pattern::{Pattern, PatternKind},
};
use minecraft_command_types::snbt::SNBTString;
use parser_rs::{
    combinators::{char, choice::choice},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

use crate::{expression::literal_expression, identifier, string, whitespace};

pub fn pattern(input: &mut Stream) -> Option<Pattern> {
    choice((
        char('_')
            .syntax(SemanticTokenKind::Variable)
            .map_to(PatternKind::Wildcard),
        |input: &mut Stream| {
            let (name_span, name) = identifier("struct or binding name")
                .spanned()
                .parse(input)?;

            let struct_fields = (|input: &mut Stream| {
                whitespace(input)?;

                char('{').parse(input)?;

                whitespace(input)?;

                let fields = (|input: &mut Stream| {
                    let (field_name_span, field_name) = identifier("field name")
                        .spanned()
                        .syntax(SemanticTokenKind::Variable)
                        .parse(input)?;

                    whitespace(input)?;

                    let pattern = (|input: &mut Stream| {
                        char(':').parse(input)?;
                        whitespace(input)?;
                        pattern(input)
                    })
                    .optional()
                    .parse(input)?;

                    Some((
                        HighSNBTString {
                            span: field_name_span,
                            snbt_string: SNBTString(false, field_name.to_string()),
                        },
                        pattern,
                    ))
                })
                .padded(whitespace)
                .separated_by::<_, Vec<_>>(char(','))
                .parse(input)?;

                char('}').parse(input)?;

                Some(fields)
            })
            .optional()
            .parse(input)?;

            Some(if let Some(fields) = struct_fields {
                input.add_syntax(name_span, SemanticTokenKind::Class);

                PatternKind::Struct(name.to_string(), fields.into_iter().collect())
            } else {
                input.add_syntax(name_span, SemanticTokenKind::Variable);

                PatternKind::Binding(name.to_string())
            })
        },
        |input: &mut Stream| {
            char('(').parse(input)?;
            whitespace(input)?;
            let elements = pattern
                .padded(whitespace)
                .separated_by(char(','))
                .parse(input)?;
            char(')').parse(input)?;

            Some(PatternKind::Tuple(elements))
        },
        |input: &mut Stream| {
            char('{').parse(input)?;
            whitespace(input)?;
            let elements: Vec<_> = (|input: &mut Stream| {
                whitespace(input)?;
                let (key_span, key) = string
                    .spanned()
                    .syntax(SemanticTokenKind::Variable)
                    .parse(input)?;
                let pattern = (|input: &mut Stream| {
                    whitespace(input)?;
                    char(':').parse(input)?;
                    whitespace(input)?;
                    pattern(input)
                })
                .optional()
                .parse(input)?;
                whitespace(input)?;

                Some((
                    HighSNBTString {
                        span: key_span,
                        snbt_string: SNBTString(false, key.to_string()),
                    },
                    pattern,
                ))
            })
            .separated_by(char(','))
            .parse(input)?;
            char('}').parse(input)?;
            Some(PatternKind::Compound(
                elements.into_iter().collect::<BTreeMap<_, _>>(),
            ))
        },
        |input: &mut Stream| {
            char('&').parse(input)?;
            let pattern = pattern(input)?;

            Some(PatternKind::Dereference(Box::new(pattern)))
        },
        literal_expression.spanned().map(|(value_span, value)| {
            PatternKind::Literal(LiteralExpression {
                span: value_span,
                kind: value,
            })
        }),
    ))
    .spanned()
    .map(|(span, kind)| Pattern { span, kind })
    .parse(input)
}
