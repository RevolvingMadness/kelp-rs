use kelp_core::high::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTExpression,
    data_type::try_parse_data_type,
    expression::{
        as_cast::lower_as_cast_expression,
        assignment::lower_assignment_expression,
        binary::lower_binary_expression,
        boolean::lower_boolean_expression,
        character::lower_character_expression,
        command::{lower_command_expression, try_parse_command_expression},
        compound::{lower_compound_expression, try_parse_compound_expression},
        data::{lower_data_expression, try_parse_data_expression},
        field_access::lower_field_access_expression,
        index::lower_index_expression,
        list::{lower_list_expression, try_parse_list_expression},
        numeric::lower_numeric_expression,
        parenthesized::lower_parenthesized_expression,
        path::lower_path_expression,
        score::{lower_score_expression, try_parse_score_expression},
        string::lower_string_expression,
        r#struct::{
            lower_struct_struct_expression, lower_tuple_struct_expression,
            try_parse_struct_struct_expression_fields, try_parse_tuple_struct_expression_fields,
        },
        to_cast::lower_to_cast_expression,
        tuple::lower_tuple_expression,
        unary::lower_unary_expression,
        underscore::lower_underscore_expression,
        unit::lower_unit_expression,
    },
    parser::Parser,
    path::generic::try_parse_generic_path,
    syntax::SyntaxKind,
};

pub mod as_cast;
pub mod assignment;
pub mod binary;
pub mod boolean;
pub mod character;
pub mod command;
pub mod compound;
pub mod data;
pub mod field_access;
pub mod index;
pub mod list;
pub mod numeric;
pub mod parenthesized;
pub mod path;
pub mod score;
pub mod string;
pub mod r#struct;
pub mod to_cast;
pub mod tuple;
pub mod unary;
pub mod underscore;
pub mod unit;

#[must_use]
pub fn is_expression_recovery(char: char) -> bool {
    char.is_alphanumeric() || char == '('
}

pub fn try_parse_expression(parser: &mut Parser) -> bool {
    try_parse_assignment(parser)
}

pub fn try_parse_assignment(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_to_cast(parser) {
        return false;
    }

    let state = parser.save_state();
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
    } else {
        parser.restore_state(state);
    }
    true
}

pub fn try_parse_to_cast(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_logical_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if parser.peek_identifier() == Some("to") {
            parser.start_node_at(checkpoint, SyntaxKind::ToCastExpression);
            parser.bump_str(SyntaxKind::ToKeyword, "to");
            parser.expect_inline_whitespace();
            parser.expect_identifier_kind(
                SyntaxKind::RuntimeStorageType,
                "Expected runtime storage type",
            );
            parser.finish_node();
            continue;
        }

        if parser.peek_identifier() == Some("as") {
            parser.start_node_at(checkpoint, SyntaxKind::AsCastExpression);
            parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
            parser.expect_inline_whitespace();
            if !try_parse_data_type(parser) {
                parser.error("Expected data type");
            }
            parser.finish_node();
            continue;
        }

        parser.restore_state(state);
        break;
    }
    true
}

pub fn try_parse_logical_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_logical_and(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::PipePipe, 2);
            parser.skip_inline_whitespace();
            try_parse_logical_and(parser);
            parser.finish_node();
        } else {
            parser.restore_state(state);
            break;
        }
    }
    true
}

pub fn try_parse_logical_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_bitwise_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::AmpersandAmpersand, 2);
            parser.skip_inline_whitespace();
            try_parse_bitwise_or(parser);
            parser.finish_node();
        } else {
            parser.restore_state(state);
            break;
        }
    }
    true
}

pub fn try_parse_bitwise_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_bitwise_and(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('|')
            && parser.peek_nth_char(1) != Some('|')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Pipe, 1);
            parser.skip_inline_whitespace();
            try_parse_bitwise_and(parser);
            parser.finish_node();
        } else {
            parser.restore_state(state);
            break;
        }
    }
    true
}

pub fn try_parse_bitwise_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_equality(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if parser.peek_char() == Some('&')
            && parser.peek_nth_char(1) != Some('&')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Ampersand, 1);
            parser.skip_inline_whitespace();
            try_parse_equality(parser);
            parser.finish_node();
        } else {
            parser.restore_state(state);
            break;
        }
    }
    true
}

pub fn try_parse_equality(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    if !try_parse_comparison(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

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
            parser.restore_state(state);
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
        let state = parser.save_state();
        parser.skip_inline_whitespace();

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
            parser.restore_state(state);
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
        let state = parser.save_state();
        parser.skip_inline_whitespace();

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
            parser.restore_state(state);
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

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if let Some(c) = parser.peek_char()
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
        } else {
            parser.restore_state(state);
            break;
        }
    }
    true
}

pub fn try_parse_factor(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_unary(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_inline_whitespace();

        if let Some(c) = parser.peek_char()
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
        } else {
            parser.restore_state(state);
            break;
        }
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
        let state = parser.save_state();
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
                if parser.peek_identifier() == Some("as") {
                    parser.start_node_at(checkpoint, SyntaxKind::AsCastExpression);
                    parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
                    parser.expect_inline_whitespace();
                    if !try_parse_data_type(parser) {
                        parser.error("Expected data type");
                    }
                    parser.finish_node();
                } else {
                    parser.restore_state(state);
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
            parser.start_node(SyntaxKind::NumericExpression);

            parser.expect_fractional_value("Expected numerical value");

            if parser
                .peek_char()
                .is_some_and(|char| char.is_ascii_alphabetic())
            {
                parser.bump_char_kind(SyntaxKind::NumericExpressionSuffix);
            }

            parser.finish_node();

            true
        }
        _ => parser.peek_identifier().is_some_and(|text| match text {
            "true" => {
                parser.start_node(SyntaxKind::BooleanExpression);
                parser.bump_identifier_kind(SyntaxKind::TrueKeyword, "true");
                parser.finish_node();

                true
            }
            "false" => {
                parser.start_node(SyntaxKind::BooleanExpression);
                parser.bump_identifier_kind(SyntaxKind::FalseKeyword, "false");
                parser.finish_node();

                true
            }
            "score" => {
                if !try_parse_score_expression(parser) {
                    parser.start_node(SyntaxKind::PathExpression);
                    parser.bump_identifier("score");
                    parser.finish_node();
                }

                true
            }
            "entity" | "block" | "storage" => {
                if !try_parse_data_expression(parser) {
                    parser.start_node(SyntaxKind::PathExpression);
                    parser.bump_identifier(text);
                    parser.finish_node();
                }

                true
            }
            _ => {
                if try_parse_command_expression(parser, text) {
                    return true;
                }

                if text == "_" {
                    parser.start_node(SyntaxKind::UnderscoreExpression);
                    parser.bump_identifier("_");
                    parser.finish_node();

                    return true;
                }

                let checkpoint = parser.checkpoint();

                if !try_parse_generic_path(parser, false) {
                    unreachable!();
                }

                let state = parser.save_state();

                parser.skip_inline_whitespace();

                match parser.peek_char() {
                    Some('{') => {
                        parser.replace_token_at(checkpoint, SyntaxKind::TypeName);
                        parser.start_node_at(checkpoint, SyntaxKind::StructStructExpression);

                        parser.bump_char();

                        parser.skip_whitespace();

                        let _ = try_parse_struct_struct_expression_fields(parser);

                        parser.skip_whitespace();

                        parser.expect_char('}', "Expected '}'");
                    }
                    Some('(') => {
                        parser.replace_token_at(checkpoint, SyntaxKind::TypeName);
                        parser.start_node_at(checkpoint, SyntaxKind::TupleStructExpression);

                        parser.bump_char();

                        parser.skip_whitespace();

                        let _ = try_parse_tuple_struct_expression_fields(parser);

                        parser.skip_whitespace();

                        parser.expect_char(')', "Expected ')'");
                    }
                    _ => {
                        parser.restore_state(state);

                        parser.start_node_at(checkpoint, SyntaxKind::PathExpression);
                    }
                }

                parser.finish_node();

                true
            }
        }),
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression(
    node: CSTExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    match node {
        CSTExpression::UnaryExpression(node) => lower_unary_expression(node, ctx),
        CSTExpression::PathExpression(node) => lower_path_expression(node, ctx),
        CSTExpression::UnderscoreExpression(node) => lower_underscore_expression(node, ctx),
        CSTExpression::BooleanExpression(node) => lower_boolean_expression(node, ctx),
        CSTExpression::NumericExpression(node) => lower_numeric_expression(node, ctx),
        CSTExpression::CharacterExpression(node) => lower_character_expression(node, ctx),
        CSTExpression::StringExpression(node) => lower_string_expression(node, ctx),
        CSTExpression::AssignmentExpression(node) => lower_assignment_expression(node, ctx),
        CSTExpression::BinaryExpression(node) => lower_binary_expression(node, ctx),
        CSTExpression::DataExpression(node) => lower_data_expression(node, ctx),
        CSTExpression::ScoreExpression(node) => lower_score_expression(node, ctx),
        CSTExpression::CommandExpression(node) => lower_command_expression(node, ctx),
        CSTExpression::UnitExpression(node) => lower_unit_expression(node, ctx),
        CSTExpression::CompoundExpression(node) => lower_compound_expression(node, ctx),
        CSTExpression::ListExpression(node) => lower_list_expression(node, ctx),
        CSTExpression::TupleExpression(node) => lower_tuple_expression(node, ctx),
        CSTExpression::ParenthesizedExpression(node) => lower_parenthesized_expression(node, ctx),
        CSTExpression::AsCastExpression(node) => lower_as_cast_expression(node, ctx),
        CSTExpression::ToCastExpression(node) => lower_to_cast_expression(node, ctx),
        CSTExpression::StructStructExpression(node) => lower_struct_struct_expression(node, ctx),
        CSTExpression::TupleStructExpression(node) => lower_tuple_struct_expression(node, ctx),
        CSTExpression::IndexExpression(node) => lower_index_expression(node, ctx),
        CSTExpression::FieldAccessExpression(node) => lower_field_access_expression(node, ctx),
    }
}
