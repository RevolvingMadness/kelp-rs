use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::CSTExpression,
    data_type::try_parse_data_type,
    expression::{
        with_block::{
            block::try_parse_block_expression,
            r#if::try_parse_if_expression,
            r#loop::{
                infinite::try_parse_infinite_loop_expression,
                iterator::try_parse_iterator_loop_expression,
                predicate::try_parse_predicate_loop_expression,
            },
            lower_expression_with_block,
        },
        without_block::{
            call::try_parse_call_arguments, command::try_parse_command_expression,
            coordinates::try_parse_coordinates_expression, data::try_parse_data_expression,
            entity_selector::try_parse_entity_selector_expression, list::try_parse_list_expression,
            lower_expression_without_block,
            resource_location::try_parse_resource_location_expression,
            r#return::try_parse_return_expression, score::try_parse_score_expression,
            r#struct::try_parse_struct_expression_fields,
        },
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    path::generic::{try_parse_generic_path, try_parse_generic_path_segment},
    syntax::SyntaxKind,
};

pub mod with_block;
pub mod without_block;

#[must_use]
pub fn is_expression_recovery(char: char) -> bool {
    char.is_alphanumeric() || char == '('
}

impl ParsableAstNode for CSTExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        try_parse_expression_with_block(parser) || try_parse_expression_without_block(parser)
    }
}

pub fn try_parse_expression_without_block(parser: &mut Parser) -> bool {
    try_parse_assignment(parser)
}

fn try_parse_assignment(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_to_cast(parser) {
        return false;
    }

    let state = parser.save_state();
    parser.skip_whitespace();

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
        (Some('='), c, _) if c != Some('=') => Some((1, SyntaxKind::Equal)),
        _ => None,
    };

    if let Some((len, kind)) = op_info {
        checkpoint.start_node(parser, SyntaxKind::AssignmentExpression);

        parser.add_token(kind, len);

        let state = parser.save_state();

        parser.skip_whitespace();

        if !try_parse_assignment(parser) {
            state.restore(parser);

            parser.recover_not_whitespace("Expected expression after assignment operator");
        }

        parser.finish_node();
    } else {
        state.restore(parser);
    }
    true
}

fn try_parse_to_cast(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_logical_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_identifier() == Some("to") {
            checkpoint.start_node(parser, SyntaxKind::ToCastExpression);
            parser.bump_str(SyntaxKind::ToKeyword, "to");
            parser.expect_whitespace();
            parser.expect_identifier_kind(
                SyntaxKind::RuntimeStorageType,
                "Expected runtime storage type",
            );
            parser.finish_node();
            continue;
        }

        if parser.peek_identifier() == Some("as") {
            checkpoint.start_node(parser, SyntaxKind::AsCastExpression);
            parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
            parser.expect_whitespace();
            if !try_parse_data_type(parser) {
                parser.error("Expected data type");
            }
            parser.finish_node();
            continue;
        }

        state.restore(parser);
        break;
    }
    true
}

fn try_parse_logical_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_logical_and(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::PipePipe, 2);
            parser.skip_whitespace();
            try_parse_logical_and(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_logical_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_bitwise_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::AmpersandAmpersand, 2);
            parser.skip_whitespace();
            try_parse_bitwise_or(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_bitwise_or(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_bitwise_and(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('|')
            && parser.peek_nth_char(1) != Some('|')
            && parser.peek_nth_char(1) != Some('=')
        {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Pipe, 1);
            parser.skip_whitespace();
            try_parse_bitwise_and(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_bitwise_and(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_equality(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('&')
            && parser.peek_nth_char(1) != Some('&')
            && parser.peek_nth_char(1) != Some('=')
        {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Ampersand, 1);
            parser.skip_whitespace();
            try_parse_equality(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_equality(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_comparison(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        let op_info = match (parser.peek_char(), parser.peek_nth_char(1)) {
            (Some('='), Some('=')) => Some((2, SyntaxKind::EqualEqual)),
            (Some('!'), Some('=')) => Some((2, SyntaxKind::ExclamationMarkEqual)),
            _ => None,
        };

        if let Some((len, kind)) = op_info {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
            try_parse_comparison(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_comparison(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_shift(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

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
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
            try_parse_shift(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_shift(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_term(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

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
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
            try_parse_term(parser);
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_term(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_factor(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if let Some(c) = parser.peek_char()
            && (c == '+' || c == '-')
            && parser.peek_nth_char(1) != Some('=')
        {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_whitespace();
            if !try_parse_factor(parser) {
                parser.recover_not_whitespace("Expected expression after operator");
            }
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_factor(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_unary(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if let Some(c) = parser.peek_char()
            && (c == '*' || c == '/' || c == '%')
            && parser.peek_nth_char(1) != Some('=')
        {
            checkpoint.start_node(parser, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_whitespace();
            if !try_parse_unary(parser) {
                parser.recover_not_whitespace("Expected expression after operator");
            }
            parser.finish_node();
        } else {
            state.restore(parser);
            break;
        }
    }
    true
}

fn try_parse_unary(parser: &mut Parser) -> bool {
    if matches!(parser.peek_char(), Some('!' | '-' | '*' | '&')) {
        parser.start_node(SyntaxKind::UnaryExpression);
        parser.bump_char();
        parser.skip_whitespace();
        try_parse_unary(parser);
        parser.finish_node();

        return true;
    }

    try_parse_postfix(parser)
}

fn try_parse_postfix(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();
    if !try_parse_primary(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        match parser.peek_char() {
            Some('[') => {
                checkpoint.start_node(parser, SyntaxKind::IndexExpression);
                parser.bump_char();
                parser.skip_whitespace();
                if !CSTExpression::try_parse(parser) {
                    parser.recover_not_whitespace("Expected index expression");
                }
                parser.skip_whitespace();
                parser.expect_char(']', "Expected closing bracket ']'");
                parser.finish_node();
            }
            Some('(') => {
                checkpoint.start_node(parser, SyntaxKind::CallExpression);
                parser.bump_char();
                parser.skip_whitespace();

                if parser.peek_char() != Some(')') {
                    try_parse_call_arguments(parser);
                }

                parser.expect_char(')', "Expected closing parenthesis ')'");
                parser.finish_node();
            }
            Some('.') => {
                let dot_state = parser.save_state();
                parser.bump_char();
                parser.skip_whitespace();

                let mut is_method_call = false;

                if try_parse_generic_path_segment(parser, true) {
                    parser.skip_whitespace();

                    if parser.peek_char() == Some('(') {
                        is_method_call = true;
                    }
                }

                dot_state.restore(parser);

                if is_method_call {
                    checkpoint.start_node(parser, SyntaxKind::MethodCallExpression);
                    parser.bump_char();
                    parser.skip_whitespace();

                    assert!(try_parse_generic_path_segment(parser, true));

                    parser.skip_whitespace();
                    parser.bump_char();
                    parser.skip_whitespace();

                    if parser.peek_char() != Some(')') {
                        try_parse_call_arguments(parser);
                    }

                    parser.expect_char(')', "Expected closing parenthesis ')'");
                } else {
                    checkpoint.start_node(parser, SyntaxKind::FieldAccessExpression);
                    parser.bump_char();
                    parser.skip_whitespace();

                    if let Some(identifier) = parser.peek_identifier() {
                        parser.add_token(SyntaxKind::FieldName, identifier.len());
                    } else if let Some(whole_value) = parser.peek_whole_value() {
                        parser.add_token(SyntaxKind::FieldName, whole_value.len());
                    } else {
                        parser.error("Expected field name");
                    }
                }

                parser.finish_node();
            }
            _ => {
                if parser.peek_identifier() == Some("as") {
                    checkpoint.start_node(parser, SyntaxKind::AsCastExpression);
                    parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
                    parser.expect_whitespace();
                    if !try_parse_data_type(parser) {
                        parser.error("Expected data type");
                    }
                    parser.finish_node();
                } else {
                    state.restore(parser);
                    break;
                }
            }
        }
    }

    true
}

fn try_parse_primary(parser: &mut Parser) -> bool {
    if try_parse_expression_with_block(parser) {
        return true;
    }

    match parser.peek_char() {
        Some('[') => try_parse_list_expression(parser),
        Some('(') => {
            let checkpoint = parser.mark();
            parser.bump_char(); // Bump '('
            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                checkpoint.start_node(parser, SyntaxKind::UnitExpression);
                parser.bump_char();
                parser.finish_node();
                return true;
            }

            if !CSTExpression::try_parse(parser) {
                parser.error("Expected expression");
            }

            parser.skip_whitespace();

            let mut is_tuple = false;
            if parser.peek_char() == Some(',') {
                is_tuple = true;

                while parser.try_bump_char(',') {
                    parser.skip_whitespace();

                    if parser.peek_char() == Some(')') {
                        break;
                    }

                    if !CSTExpression::try_parse(parser) {
                        parser.error("Expected expression after ','");
                    }
                    parser.skip_whitespace();
                }
            }

            let kind = if is_tuple {
                SyntaxKind::TupleExpression
            } else {
                SyntaxKind::ParenthesizedExpression
            };

            checkpoint.start_node(parser, kind);
            if !parser.try_bump_char(')') {
                parser.error("Expected closing parenthesis ')'");
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
        Some('/') => {
            parser.bump_char();

            parser.skip_whitespace();

            let Some(identifier) = parser.peek_identifier() else {
                parser.error("Expected command");

                return true;
            };

            if !try_parse_command_expression(parser, identifier) {
                parser.error_with_len("Unknown command", identifier.len());

                parser.bump_identifier(identifier);

                return true;
            }

            true
        }
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
        _ => {
            let Some(identifier) = parser.peek_identifier() else {
                return false;
            };

            match identifier {
                "true" => {
                    parser.start_node(SyntaxKind::BooleanExpression);
                    parser.bump_identifier_kind(SyntaxKind::TrueKeyword, "true");
                    parser.finish_node();

                    return true;
                }
                "false" => {
                    parser.start_node(SyntaxKind::BooleanExpression);
                    parser.bump_identifier_kind(SyntaxKind::FalseKeyword, "false");
                    parser.finish_node();

                    return true;
                }
                "score" => {
                    if try_parse_score_expression(parser) {
                        return true;
                    }
                }
                "entity" | "block" | "storage" => {
                    if try_parse_data_expression(parser) {
                        return true;
                    }
                }
                "resource_location" => {
                    if try_parse_resource_location_expression(parser) {
                        return true;
                    }
                }
                "entity_selector" => {
                    if try_parse_entity_selector_expression(parser) {
                        return true;
                    }
                }
                "coordinates" => {
                    if try_parse_coordinates_expression(parser) {
                        return true;
                    }
                }
                "return" => {
                    if try_parse_return_expression(parser) {
                        return true;
                    }
                }
                _ => {}
            }

            if try_parse_command_expression(parser, identifier) {
                return true;
            }

            if identifier == "_" {
                parser.start_node(SyntaxKind::UnderscoreExpression);
                parser.bump_identifier("_");
                parser.finish_node();

                return true;
            }

            let checkpoint = parser.mark();

            if !try_parse_generic_path(parser, false) {
                unreachable!();
            }

            let state = parser.save_state();
            parser.skip_whitespace();

            if parser.peek_char() == Some('{') {
                checkpoint.replace_token(parser, SyntaxKind::TypeName);
                checkpoint.start_node(parser, SyntaxKind::StructExpression);

                parser.bump_char();

                parser.skip_whitespace();

                let _ = try_parse_struct_expression_fields(parser);

                parser.skip_whitespace();

                if !parser.expect_char('}', "Expected '}'") {
                    parser.bump_until_char(&['}']);

                    parser.bump_char();
                }
            } else {
                state.restore(parser);
                checkpoint.start_node(parser, SyntaxKind::PathExpression);
            }

            parser.finish_node();

            true
        }
    }
}

pub fn try_parse_expression_with_block(parser: &mut Parser) -> bool {
    if parser.peek_char() == Some('{') {
        if !try_parse_block_expression(parser) {
            parser.error("Expected block expression");

            return false;
        }

        true
    } else {
        let state = parser.save_state();

        let Some(identifier) = parser.peek_identifier() else {
            state.restore(parser);

            return false;
        };

        let is_block_expression = match identifier {
            "if" => try_parse_if_expression(parser),
            "while" => try_parse_predicate_loop_expression(parser),
            "loop" => try_parse_infinite_loop_expression(parser),
            "for" => try_parse_iterator_loop_expression(parser),
            _ => false,
        };

        if !is_block_expression {
            state.restore(parser);
        }

        is_block_expression
    }
}

impl LowerableAstNode for CSTExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ExpressionWithBlock(node) => lower_expression_with_block(node, ctx),
            Self::ExpressionWithoutBlock(node) => lower_expression_without_block(node, ctx),
        }
    }
}
