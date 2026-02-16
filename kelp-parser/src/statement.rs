use kelp_core::statement::{Statement, StatementKind};
use parser_rs::{
    combinators::{char, choice::choice, literal},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

use crate::resource_location::parse_resource_location;
use crate::{data_type::parse_data_type, expression::expression};
use crate::{
    identifier, inline_whitespace, newline_whitespace, required_inline_whitespace, whitespace,
};
use crate::{pattern::pattern, range::parse_integer_range};

pub fn mcfunction_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("mcfn")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let id = parse_resource_location(input)?;
    whitespace(input)?;
    let (statement_span, statement_kind) = block_statement.spanned().parse(input)?;

    Some(StatementKind::MCFunction(
        id,
        Box::new(Statement {
            span: statement_span,
            kind: statement_kind,
        }),
    ))
}

pub fn while_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("while")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let condition = expression.parse(input)?;
    // Expression already consumes trailing whitespace
    let (statement_span, statement_kind) = block_statement.spanned().parse(input)?;

    Some(StatementKind::While(
        condition,
        Box::new(Statement {
            span: statement_span,
            kind: statement_kind,
        }),
    ))
}

pub fn for_in_statement(input: &mut Stream) -> Option<StatementKind> {
    let is_reversed = choice((
        literal("reverse_for").map_to(true),
        literal("for").map_to(false),
    ))
    .syntax(SemanticTokenKind::Keyword)
    .parse(input)?;

    required_inline_whitespace(input)?;
    let item = identifier("variable name")
        .syntax(SemanticTokenKind::Variable)
        .parse(input)?;
    required_inline_whitespace(input)?;
    literal("in")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let target = expression.parse(input)?;
    // Expression already consumes trailing whitespace
    let (statement_span, statement_kind) = block_statement.spanned().parse(input)?;

    Some(StatementKind::ForIn(
        is_reversed,
        item.to_string(),
        target,
        Box::new(Statement {
            span: statement_span,
            kind: statement_kind,
        }),
    ))
}

pub fn match_statement(input: &mut Stream) -> Option<StatementKind> {
    // TODO use required_inline_whitespace
    literal("match")
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
    .separated_by::<_, Vec<_>>(newline_whitespace("end of match case"))
    .parse(input)?;
    whitespace(input)?;
    char('}').parse(input)?;

    Some(StatementKind::Match(target, cases.into_iter().collect()))
}

pub fn if_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("if")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let condition = expression.parse(input)?;
    let (body_span, body_kind) = block_statement.spanned().parse(input)?;
    let else_body = (|input: &mut Stream| {
        required_inline_whitespace(input)?;
        literal("else")
            .syntax(SemanticTokenKind::Keyword)
            .parse(input)?;
        required_inline_whitespace(input)?;
        choice((block_statement, if_statement))
            .spanned()
            .parse(input)
    })
    .optional()
    .parse(input)?;

    Some(StatementKind::If(
        condition,
        Box::new(Statement {
            span: body_span,
            kind: body_kind,
        }),
        else_body.map(|(else_body_span, else_body_kind)| {
            Box::new(Statement {
                span: else_body_span,
                kind: else_body_kind,
            })
        }),
    ))
}

pub fn variable_declaration_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("let")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let pattern = pattern.parse(input)?;
    let data_type = (|input: &mut Stream| {
        inline_whitespace(input)?;
        char(':').parse(input)?;
        inline_whitespace(input)?;
        let type_ = parse_data_type(input)?;

        Some(type_)
    })
    .optional()
    .parse(input)?;
    inline_whitespace(input)?;
    char('=').parse(input)?;
    inline_whitespace(input)?;
    let value = expression.parse(input)?;

    Some(StatementKind::VariableDeclaration(
        data_type, pattern, value,
    ))
}

pub fn data_type_declaration_statement<'a>(input: &mut Stream<'a>) -> Option<StatementKind> {
    literal("type")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let name = identifier("type name")
        .syntax(SemanticTokenKind::Type)
        .parse(input)?;
    let generics: Option<Vec<_>> = (|input: &mut Stream<'a>| {
        char('<').parse(input)?;
        whitespace(input)?;
        let generics = identifier("generic parameter")
            .syntax(SemanticTokenKind::TypeParameter)
            .padded(whitespace)
            .separated_by::<_, Vec<_>>(char(','))
            .parse(input)?;
        char('>').parse(input)?;
        Some(generics)
    })
    .optional()
    .parse(input)?;
    whitespace(input)?;
    char('=').parse(input)?;
    whitespace(input)?;
    let alias = parse_data_type.parse(input)?;

    Some(StatementKind::TypeDeclaration(
        name.to_string(),
        generics
            .unwrap_or_default()
            .into_iter()
            .map(|generic| generic.to_string())
            .collect(),
        alias,
    ))
}

pub fn struct_declaration_statement<'a>(input: &mut Stream<'a>) -> Option<StatementKind> {
    literal("struct")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;

    required_inline_whitespace(input)?;

    let name = identifier("struct name")
        .syntax(SemanticTokenKind::Struct)
        .parse(input)?;

    let generics: Option<Vec<_>> = (|input: &mut Stream<'a>| {
        char('<').parse(input)?;
        whitespace(input)?;

        let generics = identifier("generic parameter")
            .syntax(SemanticTokenKind::TypeParameter)
            .padded(whitespace)
            .separated_by::<_, Vec<_>>(char(','))
            .parse(input)?;

        char('>').parse(input)?;

        Some(generics)
    })
    .optional()
    .parse(input)?;

    if generics.is_some() {
        whitespace(input)?;
    } else {
        inline_whitespace(input)?;
    }

    char('{').parse(input)?;

    let fields = (|input: &mut Stream| {
        whitespace(input)?;

        let field_name = identifier("field name")
            .syntax(SemanticTokenKind::Property)
            .parse(input)?;

        whitespace(input)?;

        char(':').parse(input)?;

        whitespace(input)?;

        let field_type = parse_data_type(input)?;

        whitespace(input)?;

        Some((field_name.to_string(), field_type))
    })
    .separated_by::<_, Vec<_>>(char(','))
    .parse(input)?;

    char('}').parse(input)?;

    Some(StatementKind::StructDeclaration(
        name.to_string(),
        generics
            .unwrap_or_default()
            .into_iter()
            .map(|generic| generic.to_string())
            .collect(),
        fields.into_iter().collect(),
    ))
}

pub fn block_statement(input: &mut Stream) -> Option<StatementKind> {
    char('{').parse(input)?;
    whitespace(input)?;
    let (statements, _) = parse_statement
        .separated_by_trailing(newline_whitespace("end of statement"))
        .parse(input)?;
    whitespace(input)?;
    char('}').parse(input)?;

    Some(StatementKind::Block(statements))
}

pub fn append_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("append")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
    required_inline_whitespace(input)?;
    let target = expression.parse(input)?;
    required_inline_whitespace(input)?;
    let source = expression.parse(input)?;

    Some(StatementKind::AppendData(target, Box::new(source)))
}

pub fn remove_statement(input: &mut Stream) -> Option<StatementKind> {
    literal("remove")
        .syntax(SemanticTokenKind::Keyword)
        .parse(input)?;
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
        struct_declaration_statement,
        data_type_declaration_statement,
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
