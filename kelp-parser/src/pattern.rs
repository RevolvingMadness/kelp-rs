use kelp_core::{
    expression::LiteralExpression,
    pattern::{Pattern, PatternKind},
};
use parser_rs::{
    combinators::{char, choice::choice},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

use crate::{expression::literal_expression, identifier, whitespace};

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
            let elements = pattern
                .padded(whitespace)
                .separated_by(char(','))
                .parse(input)?;
            char(')').parse(input)?;

            Some(PatternKind::Tuple(elements))
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
