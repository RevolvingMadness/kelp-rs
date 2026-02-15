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
        identifier("binding name")
            .syntax(SemanticTokenKind::Variable)
            .map(|binding| PatternKind::Binding(binding.to_string())),
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
                    pattern.parse(input)
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
            let pattern = pattern.parse(input)?;

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
