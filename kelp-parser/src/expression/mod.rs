use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::{
        CSTBlockExpression, CSTCallArguments, CSTCommandExpression, CSTCoordinatesExpression,
        CSTDataExpression, CSTDataType, CSTEntitySelectorExpression, CSTExpression,
        CSTExpressionWithBlock, CSTExpressionWithoutBlock, CSTGenericPath, CSTGenericPathSegment,
        CSTIfExpression, CSTInfiniteLoopExpression, CSTIteratorLoopExpression, CSTListExpression,
        CSTPredicateLoopExpression, CSTResourceLocationExpression, CSTReturnExpression,
        CSTScoreExpression, CSTStructExpressionFields,
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
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
        CSTExpressionWithoutBlock::try_parse(parser) || CSTExpressionWithBlock::try_parse(parser)
    }
}

fn try_parse_assignment(parser: &mut Parser) -> bool {
    let marker = parser.mark();
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
        marker.start_node(parser, SyntaxKind::AssignmentExpression);

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
    let marker = parser.mark();

    if !try_parse_logical_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_identifier() == Some("to") {
            marker.start_node(parser, SyntaxKind::ToCastExpression);
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
            marker.start_node(parser, SyntaxKind::AsCastExpression);
            parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
            parser.expect_whitespace();
            if !CSTDataType::try_parse(parser) {
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
    let marker = parser.mark();

    if !try_parse_logical_and(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
    if !try_parse_bitwise_or(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();

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
            marker.start_node(parser, SyntaxKind::BinaryExpression);
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
    let marker = parser.mark();
    if !try_parse_primary(parser) {
        return false;
    }

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        match parser.peek_char() {
            Some('[') => {
                marker.start_node(parser, SyntaxKind::IndexExpression);
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
                marker.start_node(parser, SyntaxKind::CallExpression);
                parser.bump_char();
                parser.skip_whitespace();

                if parser.peek_char() != Some(')') {
                    CSTCallArguments::try_parse(parser);
                }

                parser.expect_char(')', "Expected closing parenthesis ')'");
                parser.finish_node();
            }
            Some('.') => {
                let dot_state = parser.save_state();
                parser.bump_char();
                parser.skip_whitespace();

                let mut is_method_call = false;

                if CSTGenericPathSegment::try_parse(parser) {
                    parser.skip_whitespace();

                    if parser.peek_char() == Some('(') {
                        is_method_call = true;
                    }
                }

                dot_state.restore(parser);

                if is_method_call {
                    marker.start_node(parser, SyntaxKind::MethodCallExpression);
                    parser.bump_char();
                    parser.skip_whitespace();

                    assert!(CSTGenericPathSegment::try_parse(parser));

                    parser.skip_whitespace();
                    parser.bump_char();
                    parser.skip_whitespace();

                    if parser.peek_char() != Some(')') {
                        CSTCallArguments::try_parse(parser);
                    }

                    parser.expect_char(')', "Expected closing parenthesis ')'");
                } else {
                    marker.start_node(parser, SyntaxKind::FieldAccessExpression);
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
                    marker.start_node(parser, SyntaxKind::AsCastExpression);
                    parser.bump_identifier_kind(SyntaxKind::AsKeyword, "as");
                    parser.expect_whitespace();
                    if !CSTDataType::try_parse(parser) {
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
    if CSTExpressionWithBlock::try_parse(parser) {
        return true;
    }

    match parser.peek_char() {
        Some('[') => CSTListExpression::try_parse(parser),
        Some('(') => {
            let marker = parser.mark();
            parser.bump_char(); // Bump '('
            parser.skip_whitespace();

            if parser.peek_char() == Some(')') {
                marker.start_node(parser, SyntaxKind::UnitExpression);
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

            marker.start_node(parser, kind);
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

            CSTCommandExpression::expect(parser, "Expected command expression");

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
                    if CSTScoreExpression::try_parse(parser) {
                        return true;
                    }
                }
                "entity" | "block" | "storage" => {
                    if CSTDataExpression::try_parse(parser) {
                        return true;
                    }
                }
                "resource_location" => {
                    if CSTResourceLocationExpression::try_parse(parser) {
                        return true;
                    }
                }
                "entity_selector" => {
                    if CSTEntitySelectorExpression::try_parse(parser) {
                        return true;
                    }
                }
                "coordinates" => {
                    if CSTCoordinatesExpression::try_parse(parser) {
                        return true;
                    }
                }
                "return" => {
                    if CSTReturnExpression::try_parse(parser) {
                        return true;
                    }
                }
                _ => {}
            }

            if CSTCommandExpression::try_parse(parser) {
                return true;
            }

            if identifier == "_" {
                parser.start_node(SyntaxKind::UnderscoreExpression);
                parser.bump_identifier("_");
                parser.finish_node();

                return true;
            }

            let marker = parser.mark();

            if !CSTGenericPath::try_parse(parser) {
                unreachable!();
            }

            let state = parser.save_state();
            parser.skip_whitespace();

            if parser.peek_char() == Some('{') {
                marker.replace_token(parser, SyntaxKind::TypeName);
                marker.start_node(parser, SyntaxKind::StructExpression);

                parser.bump_char();

                parser.skip_whitespace();

                let _ = CSTStructExpressionFields::try_parse(parser);

                parser.skip_whitespace();

                if !parser.expect_char('}', "Expected '}'") {
                    parser.bump_until_char(&['}']);

                    parser.bump_char();
                }
            } else {
                state.restore(parser);
                marker.start_node(parser, SyntaxKind::PathExpression);
            }

            parser.finish_node();

            true
        }
    }
}

impl ParsableAstNode for CSTExpressionWithBlock {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() == Some('{') {
            return CSTBlockExpression::try_parse(parser);
        }

        let Some(identifier) = parser.peek_identifier() else {
            return false;
        };

        match identifier {
            "if" => CSTIfExpression::try_parse(parser),
            "while" => CSTPredicateLoopExpression::try_parse(parser),
            "loop" => CSTInfiniteLoopExpression::try_parse(parser),
            "for" => CSTIteratorLoopExpression::try_parse(parser),
            _ => false,
        }
    }
}

impl LowerableAstNode for CSTExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ExpressionWithBlock(node) => node.lower(ctx),
            Self::ExpressionWithoutBlock(node) => node.lower(ctx),
        }
    }
}
