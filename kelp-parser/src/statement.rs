use kelp_core::statement::{Statement, StatementKind};
use parser_rs::{FnParser, SemanticTokenKind, Stream, char, choice, literal, suggest_literal};

use crate::expression::expression;
use crate::range::parse_integer_range;
use crate::resource_location::parse_resource_location;
use crate::{
    identifier, inline_whitespace, newline_whitespace, required_inline_whitespace, whitespace,
};

pub fn mcfunction_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("mcfunction")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let id = parse_resource_location(input)?;
    required_inline_whitespace(input)?;
    let statement = parse_statement.parse(input)?;

    Some(StatementKind::MCFunction(id, Box::new(statement)))
}

pub fn while_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("while")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let condition = expression.parse(input)?;
    required_inline_whitespace(input)?;
    let statement = parse_statement.parse(input)?;

    Some(StatementKind::While(condition, Box::new(statement)))
}

pub fn for_in_statement(input: &mut Stream) -> Option<StatementKind> {
    let (is_reversed, is_string) = choice((
        suggest_literal("reverse_string_for").map_to((true, true)),
        suggest_literal("reverse_for").map_to((true, false)),
        suggest_literal("string_for").map_to((false, true)),
        suggest_literal("for").map_to((false, false)),
    ))
    .syntax(SemanticTokenKind::Keyword)
    .parse(input)?;

    required_inline_whitespace(input)?;
    let item = identifier("variable name").parse(input)?;
    required_inline_whitespace(input)?;
    suggest_literal("in")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let target = expression.parse(input)?;
    required_inline_whitespace(input)?;
    let statement = parse_statement.parse(input)?;

    Some(StatementKind::ForIn(
        is_reversed,
        is_string,
        item.to_string(),
        target,
        Box::new(statement),
    ))
}

pub fn match_statement(input: &mut Stream) -> Option<StatementKind> {
    // TODO use required_inline_whitespace
    suggest_literal("match")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let target = expression.parse(input)?;
    // Expression already consumes trailing whitespace
    char('{').parse(input)?;
    let cases = (|input: &mut Stream| {
        whitespace(input)?;
        let number = parse_integer_range.parse(input)?;
        whitespace(input)?;
        literal("->").parse(input)?;
        whitespace(input)?;
        let statement = parse_statement.parse(input)?;
        Some((number, Box::new(statement)))
    })
    .separated_by::<_, _, Vec<_>>(newline_whitespace("end of match case"))
    .parse(input)?;
    whitespace(input)?;
    char('}').parse(input)?;

    Some(StatementKind::Match(target, cases.into_iter().collect()))
}

pub fn if_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("if")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let condition = expression.parse(input)?;
    let body = parse_statement.parse(input)?;
    let else_body = (|input: &mut Stream| {
        required_inline_whitespace(input)?;
        suggest_literal("else")
            .syntax(SemanticTokenKind::Keyword)
            .parse(input)?;
        required_inline_whitespace(input)?;
        parse_statement.parse(input)
    })
    .optional()
    .parse(input)?;

    Some(StatementKind::If(
        condition,
        Box::new(body),
        else_body.map(Box::new),
    ))
}

pub fn variable_declaration_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("let")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let name = identifier("variable name")
        .syntax(SemanticTokenKind::Variable)
        .parse(input)?;
    inline_whitespace(input)?;
    char(':').parse(input)?;
    inline_whitespace(input)?;
    let type_ = identifier("variable type")
        .syntax(SemanticTokenKind::Class)
        .parse(input)?;
    inline_whitespace(input)?;
    char('=').parse(input)?;
    inline_whitespace(input)?;
    let value = expression.parse(input)?;

    Some(StatementKind::VariableDeclaration(
        type_.to_string(),
        name.to_string(),
        value,
    ))
}

pub fn block_statement(input: &mut Stream) -> Option<StatementKind> {
    char('{').parse(input)?;
    whitespace(input)?;
    let statements = parse_statement
        .separated_by_trailing(newline_whitespace("end of statement"))
        .parse(input)?;
    whitespace(input)?;
    char('}').parse(input)?;

    Some(StatementKind::Block(statements))
}

pub fn append_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("append").parse(input)?;
    required_inline_whitespace(input)?;
    let target = expression.parse(input)?;
    required_inline_whitespace(input)?;
    let source = expression.parse(input)?;

    Some(StatementKind::AppendData(target, Box::new(source)))
}

pub fn remove_statement(input: &mut Stream) -> Option<StatementKind> {
    suggest_literal("remove").parse(input)?;
    required_inline_whitespace(input)?;
    let expression = expression.parse(input)?;

    Some(StatementKind::RemoveData(expression))
}

#[inline]
pub fn expression_statement(input: &mut Stream) -> Option<StatementKind> {
    expression.map(StatementKind::Expression).parse(input)
}

pub fn parse_statement(input: &mut Stream) -> Option<Statement> {
    let (span, statement) = choice((
        mcfunction_statement,
        while_statement,
        for_in_statement,
        match_statement,
        if_statement,
        variable_declaration_statement,
        block_statement,
        append_statement,
        remove_statement,
        expression_statement,
    ))
    .spanned()
    .label("statement")
    .parse(input)?;

    Some(Statement::new(span, statement))
}
