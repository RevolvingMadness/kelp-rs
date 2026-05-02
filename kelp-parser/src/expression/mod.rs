use kelp_core::high::{expression::Expression, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTExpression,
    data_type::try_parse_data_type,
    entity_selector::try_parse_entity_selector,
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
            command::try_parse_command_expression, data::try_parse_data_expression,
            list::try_parse_list_expression, lower_expression_without_block,
            r#return::try_parse_return_expression, score::try_parse_score_expression,
            r#struct::try_parse_struct_expression_fields,
        },
    },
    parser::Parser,
    path::generic::try_parse_generic_path,
    resource_location::try_parse_resource_location,
    syntax::SyntaxKind,
};

pub mod with_block;
pub mod without_block;

#[must_use]
pub fn is_expression_recovery(char: char) -> bool {
    char.is_alphanumeric() || char == '('
}

pub fn try_parse_expression(parser: &mut Parser) -> bool {
    try_parse_expression_with_block(parser) || try_parse_expression_without_block(parser)
}

pub fn try_parse_expression_without_block(parser: &mut Parser) -> bool {
    try_parse_assignment(parser)
}

pub fn try_parse_assignment(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
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
        (Some('>'), Some('<'), _) => Some((2, SyntaxKind::RightArrowLeftArrow)),
        (Some('='), c, _) if c != Some('=') => Some((1, SyntaxKind::Equal)),
        _ => None,
    };

    if let Some((len, kind)) = op_info {
        parser.start_node_at(checkpoint, SyntaxKind::AssignmentExpression);

        parser.add_token(kind, len);

        let state = parser.save_state();

        parser.skip_whitespace();

        if !try_parse_assignment(parser) {
            parser.restore_state(state);

            parser.recover_not_whitespace("Expected expression after assignment operator");
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
        parser.skip_whitespace();

        if parser.peek_identifier() == Some("to") {
            parser.start_node_at(checkpoint, SyntaxKind::ToCastExpression);
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
            parser.start_node_at(checkpoint, SyntaxKind::AsCastExpression);
            parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
            parser.expect_whitespace();
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
        parser.skip_whitespace();

        if parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::PipePipe, 2);
            parser.skip_whitespace();
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
        parser.skip_whitespace();

        if parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::AmpersandAmpersand, 2);
            parser.skip_whitespace();
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
        parser.skip_whitespace();

        if parser.peek_char() == Some('|')
            && parser.peek_nth_char(1) != Some('|')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Pipe, 1);
            parser.skip_whitespace();
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
        parser.skip_whitespace();

        if parser.peek_char() == Some('&')
            && parser.peek_nth_char(1) != Some('&')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Ampersand, 1);
            parser.skip_whitespace();
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
        parser.skip_whitespace();

        let op_info = match (parser.peek_char(), parser.peek_nth_char(1)) {
            (Some('='), Some('=')) => Some((2, SyntaxKind::EqualEqual)),
            (Some('!'), Some('=')) => Some((2, SyntaxKind::ExclamationMarkEqual)),
            _ => None,
        };

        if let Some((len, kind)) = op_info {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
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
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
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
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(kind, len);
            parser.skip_whitespace();
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
        parser.skip_whitespace();

        if let Some(c) = parser.peek_char()
            && (c == '+' || c == '-')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_whitespace();
            if !try_parse_factor(parser) {
                parser.recover_not_whitespace("Expected expression after operator");
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
        parser.skip_whitespace();

        if let Some(c) = parser.peek_char()
            && (c == '*' || c == '/' || c == '%')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_whitespace();
            if !try_parse_unary(parser) {
                parser.recover_not_whitespace("Expected expression after operator");
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
        parser.skip_whitespace();
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
        parser.skip_whitespace();

        match parser.peek_char() {
            Some('[') => {
                parser.start_node_at(checkpoint, SyntaxKind::IndexExpression);
                parser.bump_char();
                parser.skip_whitespace();
                if !try_parse_expression(parser) {
                    parser.recover_not_whitespace("Expected index expression");
                }
                parser.skip_whitespace();
                parser.expect_char(']', "Expected closing bracket ']'");
                parser.finish_node();
            }
            Some('(') => {
                parser.start_node_at(checkpoint, SyntaxKind::CallExpression);
                parser.bump_char();
                parser.skip_whitespace();

                if parser.peek_char() != Some(')') {
                    loop {
                        if !try_parse_expression(parser) {
                            break;
                        }

                        parser.skip_whitespace();

                        if !parser.try_bump_char(',') {
                            break;
                        }

                        parser.skip_whitespace();

                        if parser.peek_char() == Some(')') {
                            break;
                        }
                    }
                }

                parser.expect_char(')', "Expected closing parenthesis ')'");
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
                    parser.expect_whitespace();
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

pub fn try_parse_resource_location_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::ResourceLocationExpression);

    parser.bump_identifier_kind(SyntaxKind::ResourceLocationKeyword, "resource_location");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.finish_node();

    true
}

pub fn try_parse_entity_selector_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::EntitySelectorExpression);

    parser.bump_identifier_kind(SyntaxKind::EntitySelectorKeyword, "entity_selector");

    parser.skip_whitespace();

    if !parser.try_bump_char(':') {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if !try_parse_entity_selector(parser) {
        parser.error("Expected entity selector");
    }

    parser.finish_node();

    true
}

pub fn try_parse_primary(parser: &mut Parser) -> bool {
    if try_parse_expression_with_block(parser) {
        return true;
    }

    match parser.peek_char() {
        Some('[') => try_parse_list_expression(parser),
        Some('(') => {
            let checkpoint = parser.checkpoint();
            parser.bump_char(); // Bump '('
            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                parser.start_node_at(checkpoint, SyntaxKind::UnitExpression);
                parser.bump_char();
                parser.finish_node();
                return true;
            }

            if !try_parse_expression(parser) {
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

                    if !try_parse_expression(parser) {
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

            parser.start_node_at(checkpoint, kind);
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

            let checkpoint = parser.checkpoint();

            if !try_parse_generic_path(parser, false) {
                unreachable!();
            }

            parser.skip_whitespace();

            if parser.try_bump_char('{') {
                parser.replace_token_at(checkpoint, SyntaxKind::TypeName);
                parser.start_node_at(checkpoint, SyntaxKind::StructExpression);

                parser.bump_char();

                parser.skip_whitespace();

                let _ = try_parse_struct_expression_fields(parser);

                parser.skip_whitespace();

                if !parser.expect_char('}', "Expected '}'") {
                    parser.bump_until_char(&['}']);

                    parser.bump_char();
                }
            } else {
                parser.start_node_at(checkpoint, SyntaxKind::PathExpression);
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
            parser.restore_state(state);

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
            parser.restore_state(state);
        }

        is_block_expression
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression(
    node: CSTExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    match node {
        CSTExpression::ExpressionWithBlock(node) => lower_expression_with_block(node, ctx),
        CSTExpression::ExpressionWithoutBlock(node) => lower_expression_without_block(node, ctx),
    }
}
