use kelp_core::expression::{
    Expression, ExpressionKind, constant::ConstantExpressionKind, literal::LiteralExpressionKind,
};
use ordered_float::NotNan;

use crate::{
    cst::CSTExpression,
    lower::{
        data_type::{generics::try_parse_generic_data_types, try_parse_data_type},
        expression::{
            as_cast::lower_as_cast_expression,
            assignment::lower_assignment_expression,
            binary::lower_binary_expression,
            character::lower_character_expression,
            command::{
                function::{
                    lower_function_command_expression, try_parse_function_command_expression,
                },
                r#return::{lower_return_command_expression, try_parse_return_command_expression},
                tellraw::{lower_tellraw_command_expression, try_parse_tellraw_command_expression},
            },
            compound::{lower_compound_expression, try_parse_compound_expression},
            data::{lower_data_expression, try_parse_data_expression},
            field_access::lower_field_access_expression,
            index::lower_index_expression,
            list::{lower_list_expression, try_parse_list_expression},
            parenthesized::lower_parenthesized_expression,
            score::{lower_score_expression, try_parse_score_expression},
            string::lower_string_expression,
            r#struct::lower_struct_expression,
            to_cast::lower_to_cast_expression,
            tuple::lower_tuple_expression,
            unary::lower_unary_expression,
            unit::lower_unit_expression,
            variable::lower_variable_expression,
        },
        statement::struct_declaration::bump_until_next_field_or_end,
    },
    parser::Parser,
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub mod as_cast;
pub mod assignment;
pub mod binary;
pub mod character;
pub mod command;
pub mod compound;
pub mod data;
pub mod field_access;
pub mod index;
pub mod list;
pub mod parenthesized;
pub mod score;
pub mod string;
pub mod r#struct;
pub mod to_cast;
pub mod tuple;
pub mod unary;
pub mod unit;
pub mod variable;

#[must_use]
pub fn is_expression_recovery(char: char) -> bool {
    char.is_alphanumeric() || char == '('
}

pub fn try_parse_expression(parser: &mut Parser) -> bool {
    try_parse_assignment(parser)
}

pub fn try_parse_assignment(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_logical_or(parser) {
        return false;
    }

    parser.skip_inline_whitespace();

    let op_info = match (
        parser.peek_char(),
        parser.peek_nth_char(1),
        parser.peek_nth_char(2),
    ) {
        (Some('+'), Some('='), _) => Some((2, SyntaxKind::PlusEqual)),
        (Some('-'), Some('='), _) => Some((2, SyntaxKind::MinusEqual)),
        (Some('*'), Some('='), _) => Some((2, SyntaxKind::StarEqual)),
        (Some('/'), Some('='), _) => Some((2, SyntaxKind::ForwardSlashEqual)),
        (Some('%'), Some('='), _) => Some((2, SyntaxKind::PercentEqual)),
        (Some('&'), Some('='), _) => Some((2, SyntaxKind::AmpersandEqual)),
        (Some('|'), Some('='), _) => Some((2, SyntaxKind::PipeEqual)),
        (Some('<'), Some('<'), Some('=')) => Some((3, SyntaxKind::LeftArrowLeftArrowEqual)),
        (Some('>'), Some('>'), Some('=')) => Some((3, SyntaxKind::RightArrowRightArrowEqual)),
        (Some('>'), Some('<'), _) => Some((2, SyntaxKind::RightArrowLeftArrow)),
        (Some('='), c, _) if c != Some('=') => Some((1, SyntaxKind::Equal)),
        _ => None,
    };

    if let Some((len, kind)) = op_info {
        parser.start_node_at(checkpoint, SyntaxKind::AssignmentExpression);

        parser.add_token(kind, len);

        parser.skip_inline_whitespace();

        if !try_parse_assignment(parser) {
            parser.recover_newline("Expected expression after assignment operator");
        }

        parser.finish_node();
    }
    true
}

pub fn try_parse_logical_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_logical_and(parser) {
        return false;
    }

    while parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.add_token(SyntaxKind::PipePipe, 2);
        parser.skip_inline_whitespace();
        try_parse_logical_and(parser);
        parser.finish_node();
    }
    true
}

pub fn try_parse_logical_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_bitwise_or(parser) {
        return false;
    }
    while parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.add_token(SyntaxKind::AmpersandAmpersand, 2);
        parser.skip_inline_whitespace();
        try_parse_bitwise_or(parser);
        parser.finish_node();
    }
    true
}

pub fn try_parse_bitwise_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_bitwise_and(parser) {
        return false;
    }
    while parser.peek_char() == Some('|')
        && parser.peek_nth_char(1) != Some('|')
        && parser.peek_nth_char(1) != Some('=')
    {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.add_token(SyntaxKind::Pipe, 1);
        parser.skip_inline_whitespace();
        try_parse_bitwise_and(parser);
        parser.finish_node();
    }
    true
}

pub fn try_parse_bitwise_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_equality(parser) {
        return false;
    }
    while parser.peek_char() == Some('&')
        && parser.peek_nth_char(1) != Some('&')
        && parser.peek_nth_char(1) != Some('=')
    {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.add_token(SyntaxKind::Ampersand, 1);
        parser.skip_inline_whitespace();
        try_parse_equality(parser);
        parser.finish_node();
    }
    true
}

pub fn try_parse_equality(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_comparison(parser) {
        return false;
    }
    loop {
        let op_info = match (parser.peek_char(), parser.peek_nth_char(1)) {
            (Some('='), Some('=')) => Some((2, SyntaxKind::EqualEqual)),
            (Some('!'), Some('=')) => Some((2, SyntaxKind::ExclamationMarkEqual)),
            _ => None,
        };

        if let Some((len, kind)) = op_info {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_inline_whitespace();
            try_parse_comparison(parser);
            parser.finish_node();
        } else {
            break;
        }
    }
    true
}

pub fn try_parse_comparison(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_shift(parser) {
        return false;
    }
    loop {
        let op_info = match (parser.peek_char(), parser.peek_nth_char(1)) {
            (Some('>'), Some('=')) => Some((2, SyntaxKind::RightArrowEqual)),
            (Some('<'), Some('=')) => Some((2, SyntaxKind::LeftArrowEqual)),
            (Some('>'), c) if c != Some('>') && c != Some('<') && c != Some('=') => {
                Some((1, SyntaxKind::RightArrow))
            }
            (Some('<'), c) if c != Some('<') && c != Some('=') => Some((1, SyntaxKind::LeftArrow)),
            _ => None,
        };

        if let Some((len, kind)) = op_info {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_inline_whitespace();
            try_parse_shift(parser);
            parser.finish_node();
        } else {
            break;
        }
    }
    true
}

pub fn try_parse_shift(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_term(parser) {
        return false;
    }
    loop {
        let op_info = match (
            parser.peek_char(),
            parser.peek_nth_char(1),
            parser.peek_nth_char(2),
        ) {
            (Some('<'), Some('<'), c) if c != Some('=') => {
                Some((2, SyntaxKind::LeftArrowLeftArrow))
            }
            (Some('>'), Some('>'), c) if c != Some('=') => {
                Some((2, SyntaxKind::RightArrowRightArrow))
            }
            _ => None,
        };

        if let Some((len, kind)) = op_info {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_inline_whitespace();
            try_parse_term(parser);
            parser.finish_node();
        } else {
            break;
        }
    }
    true
}

pub fn try_parse_term(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_factor(parser) {
        return false;
    }
    while let Some(c) = parser.peek_char()
        && (c == '+' || c == '-')
        && parser.peek_nth_char(1) != Some('=')
    {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.bump_char();
        parser.skip_inline_whitespace();
        if !try_parse_factor(parser) {
            parser.recover_newline("Expected expression after operator");
        }
        parser.finish_node();
    }
    true
}

pub fn try_parse_factor(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_unary(parser) {
        return false;
    }

    while let Some(c) = parser.peek_char()
        && (c == '*' || c == '/' || c == '%')
        && parser.peek_nth_char(1) != Some('=')
    {
        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
        parser.bump_char();
        parser.skip_inline_whitespace();
        if !try_parse_unary(parser) {
            parser.recover_newline("Expected expression after operator");
        }
        parser.finish_node();
    }
    true
}

pub fn try_parse_unary(parser: &mut Parser) -> bool {
    if matches!(parser.peek_char(), Some('!' | '-' | '*' | '&')) {
        parser.start_node(SyntaxKind::UnaryExpression);
        parser.bump_char();
        parser.skip_inline_whitespace();
        try_parse_unary(parser);
        parser.finish_node();

        return true;
    }

    try_parse_postfix(parser)
}

pub fn try_parse_postfix(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_primary(parser) {
        return false;
    }

    loop {
        parser.skip_inline_whitespace();

        match parser.peek_char() {
            Some('[') => {
                parser.start_node_at(checkpoint, SyntaxKind::IndexExpression);
                parser.bump_char();
                parser.skip_whitespace();
                if !try_parse_expression(parser) {
                    parser.recover_newline("Expected index expression");
                }
                parser.skip_whitespace();
                parser.expect_char(']', "Expected closing bracket ']'");
                parser.finish_node();
            }
            Some('.') => {
                parser.start_node_at(checkpoint, SyntaxKind::FieldAccessExpression);
                parser.bump_char();
                parser.skip_whitespace();
                if let Some(identifier) = parser.peek_identifier() {
                    parser.add_token(SyntaxKind::FieldName, identifier.len());
                } else if let Some(whole_value) = parser.peek_whole_value() {
                    parser.add_token(SyntaxKind::FieldName, whole_value.len());
                } else {
                    parser.error("Expected field name");
                }
                parser.finish_node();
            }
            _ => {
                let Some(id) = parser.peek_identifier() else {
                    break;
                };

                if id == "to" {
                    parser.start_node_at(checkpoint, SyntaxKind::ToCastExpression);
                    parser.bump_str(SyntaxKind::ToKeyword, "to");
                    parser.expect_inline_whitespace();
                    parser.expect_identifier_kind(
                        SyntaxKind::RuntimeStorageType,
                        "Expected runtime storage type",
                    );
                    parser.finish_node();
                } else if id == "as" {
                    parser.start_node_at(checkpoint, SyntaxKind::AsCastExpression);
                    parser.bump_identifier("as");
                    parser.expect_inline_whitespace();
                    if !try_parse_data_type(parser) {
                        parser.error("Expected data type");
                    }
                    parser.finish_node();
                } else {
                    break;
                }
            }
        }
    }
    true
}

pub fn try_parse_primary(parser: &mut Parser) -> bool {
    match parser.peek_char() {
        Some('{') => try_parse_compound_expression(parser),
        Some('[') => try_parse_list_expression(parser),
        Some('(') => {
            let checkpoint = parser.checkpoint();
            parser.bump_char();
            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                parser.start_node_at(checkpoint, SyntaxKind::UnitExpression);
                parser.bump_char();
                parser.finish_node();
                return true;
            }

            let should_emit_error = try_parse_expression(parser);
            let first_expression_pos = parser.pos;

            parser.skip_whitespace();

            let mut is_tuple = false;

            let mut emitted_error = !should_emit_error;

            if parser.peek_char() == Some(',') {
                is_tuple = true;

                while parser.try_bump_char(',') {
                    parser.skip_whitespace();

                    if parser.peek_char() == Some(')') {
                        break;
                    }

                    if !try_parse_expression(parser) {
                        if !emitted_error {
                            parser.error_at(first_expression_pos, "Expected expression");

                            emitted_error = true;
                        }

                        parser.error("Expected expression");
                    }

                    parser.skip_whitespace();
                }
            }

            let kind = if is_tuple {
                SyntaxKind::TupleExpression
            } else {
                SyntaxKind::ParenthesizedExpression
            };

            parser.start_node_at(checkpoint, kind);
            if !parser.try_bump_char(')') {
                if emitted_error {
                    parser.error("Expected ')'");
                } else {
                    parser.error("Expected expression or ')'");
                }
            }
            parser.finish_node();

            true
        }
        Some('\'') => parser.peek_quoted_char().is_some_and(|text| {
            parser.start_node(SyntaxKind::CharacterExpression);
            parser.add_token(SyntaxKind::CharacterLiteral, text.len());
            parser.finish_node();

            true
        }),
        Some('"') => parser.peek_quoted_string().is_some_and(|text| {
            parser.start_node(SyntaxKind::StringExpression);
            parser.add_token(SyntaxKind::StringLiteral, text.len());
            parser.finish_node();

            true
        }),
        Some(char) if char.is_ascii_digit() => {
            let (is_value_fractional, text) = parser.peek_fractional_value().unwrap();

            let (has_suffix, is_suffix_fractional, kind) = match parser.peek_nth_char(text.len()) {
                Some('b' | 'B') => (true, false, (SyntaxKind::ByteExpression)),
                Some('s' | 'S') => (true, false, (SyntaxKind::ShortExpression)),
                Some('i' | 'I') => (true, false, (SyntaxKind::IntegerExpression)),
                Some('l' | 'L') => (true, false, (SyntaxKind::LongExpression)),
                Some('f' | 'F') => (true, true, (SyntaxKind::FloatExpression)),
                Some('d' | 'D') => (true, true, (SyntaxKind::DoubleExpression)),
                _ => (
                    false,
                    false,
                    if is_value_fractional {
                        SyntaxKind::FloatExpression
                    } else {
                        SyntaxKind::IntegerExpression
                    },
                ),
            };

            let inner_kind = if is_value_fractional || is_suffix_fractional {
                SyntaxKind::FractionalValue
            } else {
                SyntaxKind::WholeValue
            };

            parser.start_node(kind);
            parser.add_token(inner_kind, text.len());

            if has_suffix {
                parser.bump_char_kind(SyntaxKind::NumericExpressionSuffix);
            }

            parser.finish_node();

            true
        }
        _ => parser.peek_identifier().is_some_and(|text| match text {
            "true" | "false" => {
                parser.start_node(SyntaxKind::BooleanExpression);
                parser.bump_identifier(text);
                parser.finish_node();

                true
            }
            "tellraw" => {
                if !try_parse_tellraw_command_expression(parser) {
                    parser.start_node(SyntaxKind::VariableExpression);
                    parser.bump_identifier("tellraw");
                    parser.finish_node();
                }

                true
            }
            "function" => {
                if !try_parse_function_command_expression(parser) {
                    parser.start_node(SyntaxKind::VariableExpression);
                    parser.bump_identifier("function");
                    parser.finish_node();
                }

                true
            }
            "return" => {
                if !try_parse_return_command_expression(parser) {
                    parser.start_node(SyntaxKind::VariableExpression);
                    parser.bump_identifier("return");
                    parser.finish_node();
                }

                true
            }
            "score" => {
                if !try_parse_score_expression(parser) {
                    parser.start_node(SyntaxKind::VariableExpression);
                    parser.bump_identifier("score");
                    parser.finish_node();
                }

                true
            }
            "entity" | "block" | "storage" => {
                if !try_parse_data_expression(parser) {
                    parser.start_node(SyntaxKind::VariableExpression);
                    parser.bump_identifier(text);
                    parser.finish_node();
                }

                true
            }
            _ => {
                let checkpoint = parser.checkpoint();
                parser.bump_identifier(text);

                let state = parser.save_state();

                let parsed_struct = (|| {
                    let _ = try_parse_generic_data_types(parser);

                    parser.skip_inline_whitespace();

                    if parser.peek_char() == Some('{') {
                        parser.bump_char();
                        parser.skip_whitespace();
                        while parser.peek_char() != Some('}') {
                            parser.start_node(SyntaxKind::StructExpressionField);

                            if !parser.expect_identifier_kind(
                                SyntaxKind::StructFieldName,
                                "Expected struct field name",
                            ) {
                                bump_until_next_field_or_end(parser);
                                parser.finish_node();

                                continue;
                            }

                            parser.skip_whitespace();

                            parser.expect_char(':', "Expected ':'");

                            parser.skip_whitespace();

                            if !try_parse_expression(parser) {
                                parser.error("Expected expression");
                                bump_until_next_field_or_end(parser);
                                parser.finish_node();
                                continue;
                            }
                            parser.finish_node();

                            parser.skip_whitespace();
                            if parser.try_bump_char(',') {
                                parser.skip_whitespace();
                            } else {
                                break;
                            }
                        }

                        if parser.expect_char('}', "Expected '}'") {
                            return true;
                        }
                    }
                    false
                })();

                if parsed_struct {
                    parser.replace_token_at(checkpoint, SyntaxKind::StructName);
                    parser.start_node_at(checkpoint, SyntaxKind::StructExpression);
                } else {
                    parser.restore_state(state);
                    parser.start_node_at(checkpoint, SyntaxKind::VariableExpression);
                }

                parser.finish_node();

                true
            }
        }),
    }
}

macro_rules! lower_numerical_expression {
    ($node: expr, $get_token_method_name: ident, $type_name: ty, $enum_name: ident) => {{
        let span = span_of_cst_node(&$node);

        let value_token = $node.$get_token_method_name()?;
        let value_text = value_token.text();
        let value = value_text.parse::<$type_name>().ok()?;

        Some(
            ExpressionKind::Constant(
                ConstantExpressionKind::Literal(
                    LiteralExpressionKind::$enum_name(value).with_span(span),
                )
                .with_span(span),
            )
            .with_span(span),
        )
    }};
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression(node: CSTExpression) -> Option<Expression> {
    match node {
        CSTExpression::UnaryExpression(node) => lower_unary_expression(node),
        CSTExpression::VariableExpression(node) => lower_variable_expression(node),
        CSTExpression::ByteExpression(node) => {
            lower_numerical_expression!(node, whole_value_token, i8, Byte)
        }
        CSTExpression::ShortExpression(node) => {
            lower_numerical_expression!(node, whole_value_token, i16, Short)
        }
        CSTExpression::IntegerExpression(node) => {
            lower_numerical_expression!(node, whole_value_token, i32, Integer)
        }
        CSTExpression::LongExpression(node) => {
            lower_numerical_expression!(node, whole_value_token, i64, Long)
        }
        CSTExpression::FloatExpression(node) => {
            lower_numerical_expression!(node, fractional_value_token, NotNan<f32>, Float)
        }
        CSTExpression::DoubleExpression(node) => {
            lower_numerical_expression!(node, fractional_value_token, NotNan<f64>, Double)
        }
        CSTExpression::CharacterExpression(node) => lower_character_expression(node),
        CSTExpression::StringExpression(node) => lower_string_expression(node),
        CSTExpression::AssignmentExpression(node) => lower_assignment_expression(node),
        CSTExpression::BinaryExpression(node) => lower_binary_expression(node),
        CSTExpression::DataExpression(node) => lower_data_expression(node),
        CSTExpression::ScoreExpression(node) => lower_score_expression(node),
        CSTExpression::TellrawCommandExpression(node) => lower_tellraw_command_expression(node),
        CSTExpression::FunctionCommandExpression(node) => lower_function_command_expression(node),
        CSTExpression::ReturnCommandExpression(node) => lower_return_command_expression(node),
        CSTExpression::UnitExpression(node) => lower_unit_expression(node),
        CSTExpression::CompoundExpression(node) => lower_compound_expression(node),
        CSTExpression::ListExpression(node) => lower_list_expression(node),
        CSTExpression::TupleExpression(node) => lower_tuple_expression(node),
        CSTExpression::ParenthesizedExpression(node) => lower_parenthesized_expression(node),
        CSTExpression::AsCastExpression(node) => lower_as_cast_expression(node),
        CSTExpression::ToCastExpression(node) => lower_to_cast_expression(node),
        CSTExpression::StructExpression(node) => lower_struct_expression(node),
        CSTExpression::IndexExpression(node) => lower_index_expression(node),
        CSTExpression::FieldAccessExpression(node) => lower_field_access_expression(node),
    }
}
