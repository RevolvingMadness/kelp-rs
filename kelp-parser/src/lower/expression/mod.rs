use kelp_core::{
    expression::{
        Expression, ExpressionKind, constant::ConstantExpressionKind,
        literal::LiteralExpressionKind,
    },
    high::{command::HighCommand, player_score::HighPlayerScore, snbt_string::HighSNBTString},
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    runtime_storage_type::RuntimeStorageType,
    span::Span,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cstlib::CSTNodeType,
    lower::{
        data_type::CSTDataType,
        expression::{
            as_cast::CSTAsCastExpression,
            assignment::CSTAssignmentExpression,
            binary::CSTBinaryExpression,
            char::CSTCharExpression,
            command::tellraw::CSTTellrawCommandExpression,
            compound::CSTCompoundExpression,
            data::CSTDataExpression,
            field_access::CSTFieldAccessExpression,
            index::CSTIndexExpression,
            list::CSTListExpression,
            numeric::{
                fractional::{double::CSTDoubleExpression, float::CSTFloatExpression},
                whole::{
                    byte::CSTByteExpression, integer::CSTIntegerExpression,
                    long::CSTLongExpression, short::CSTShortExpression,
                },
            },
            parenthesized::CSTParenthesizedExpression,
            score::CSTScoreExpression,
            string::CSTStringExpression,
            r#struct::CSTStructExpression,
            to_cast::CSTToCastExpression,
            tuple::CSTTupleExpression,
            unary::CSTUnaryExpression,
            unit::CSTUnitExpression,
            variable::CSTVariableExpression,
        },
    },
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

pub mod as_cast;
pub mod assignment;
pub mod binary;
pub mod char;
pub mod command;
pub mod compound;
pub mod data;
pub mod field_access;
pub mod index;
pub mod list;
pub mod numeric;
pub mod parenthesized;
pub mod score;
pub mod string;
pub mod r#struct;
pub mod to_cast;
pub mod tuple;
pub mod unary;
pub mod unit;
pub mod variable;

#[derive(Debug)]
pub enum CSTExpressionKind<'a> {
    Unary(CSTUnaryExpression<'a>),
    Assignment(CSTAssignmentExpression<'a>),
    Binary(CSTBinaryExpression<'a>),
    Data(CSTDataExpression<'a>),
    Score(CSTScoreExpression<'a>),
    TellrawCommand(CSTTellrawCommandExpression<'a>),
    Unit(CSTUnitExpression<'a>),
    Compound(CSTCompoundExpression<'a>),
    List(CSTListExpression<'a>),
    Index(CSTIndexExpression<'a>),
    Tuple(CSTTupleExpression<'a>),
    Paren(CSTParenthesizedExpression<'a>),
    ToCast(CSTToCastExpression<'a>),
    AsCast(CSTAsCastExpression<'a>),
    Variable(CSTVariableExpression<'a>),
    Byte(CSTByteExpression<'a>),
    Short(CSTShortExpression<'a>),
    Integer(CSTIntegerExpression<'a>),
    Long(CSTLongExpression<'a>),
    Float(CSTFloatExpression<'a>),
    Double(CSTDoubleExpression<'a>),
    Char(CSTCharExpression<'a>),
    String(CSTStringExpression<'a>),
    FieldAccess(CSTFieldAccessExpression<'a>),
    Struct(CSTStructExpression<'a>),
}

impl<'a> CSTExpressionKind<'a> {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> CSTExpression<'a> {
        CSTExpression { span, kind: self }
    }
}

#[derive(Debug)]
pub struct CSTExpression<'a> {
    pub span: Span,
    pub kind: CSTExpressionKind<'a>,
}

impl<'a> CSTExpression<'a> {
    #[must_use]
    pub fn is_recovery(char: char) -> bool {
        char.is_alphanumeric() || char == '('
    }

    #[must_use]
    pub fn try_parse(parser: &mut Parser) -> bool {
        Self::try_parse_expression(parser)
    }

    fn try_parse_expression(parser: &mut Parser) -> bool {
        Self::try_parse_assignment(parser)
    }

    fn try_parse_assignment(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_logical_or(parser) {
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

            if !Self::try_parse_assignment(parser) {
                parser.recover_newline("Expected expression after assignment operator");
            }

            parser.finish_node();
        }
        true
    }

    fn try_parse_logical_or(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();

        if !Self::try_parse_logical_and(parser) {
            return false;
        }

        while parser.peek_char() == Some('|') && parser.peek_nth_char(1) == Some('|') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::PipePipe, 2);
            parser.skip_inline_whitespace();
            Self::try_parse_logical_and(parser);
            parser.finish_node();
        }
        true
    }

    fn try_parse_logical_and(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_bitwise_or(parser) {
            return false;
        }
        while parser.peek_char() == Some('&') && parser.peek_nth_char(1) == Some('&') {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::AmpersandAmpersand, 2);
            parser.skip_inline_whitespace();
            Self::try_parse_bitwise_or(parser);
            parser.finish_node();
        }
        true
    }

    fn try_parse_bitwise_or(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_bitwise_and(parser) {
            return false;
        }
        while parser.peek_char() == Some('|')
            && parser.peek_nth_char(1) != Some('|')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Pipe, 1);
            parser.skip_inline_whitespace();
            Self::try_parse_bitwise_and(parser);
            parser.finish_node();
        }
        true
    }

    fn try_parse_bitwise_and(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_equality(parser) {
            return false;
        }
        while parser.peek_char() == Some('&')
            && parser.peek_nth_char(1) != Some('&')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.add_token(SyntaxKind::Ampersand, 1);
            parser.skip_inline_whitespace();
            Self::try_parse_equality(parser);
            parser.finish_node();
        }
        true
    }

    fn try_parse_equality(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_comparison(parser) {
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
                Self::try_parse_comparison(parser);
                parser.finish_node();
            } else {
                break;
            }
        }
        true
    }

    fn try_parse_comparison(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_shift(parser) {
            return false;
        }
        loop {
            let op_info = match (parser.peek_char(), parser.peek_nth_char(1)) {
                (Some('>'), Some('=')) => Some((2, SyntaxKind::RightArrowEqual)),
                (Some('<'), Some('=')) => Some((2, SyntaxKind::LeftArrowEqual)),
                (Some('>'), c) if c != Some('>') && c != Some('<') && c != Some('=') => {
                    Some((1, SyntaxKind::RightArrow))
                }
                (Some('<'), c) if c != Some('<') && c != Some('=') => {
                    Some((1, SyntaxKind::LeftArrow))
                }
                _ => None,
            };

            if let Some((len, kind)) = op_info {
                parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
                parser.add_token(kind, len);
                parser.skip_inline_whitespace();
                Self::try_parse_shift(parser);
                parser.finish_node();
            } else {
                break;
            }
        }
        true
    }

    fn try_parse_shift(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_term(parser) {
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
                Self::try_parse_term(parser);
                parser.finish_node();
            } else {
                break;
            }
        }
        true
    }

    fn try_parse_term(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_factor(parser) {
            return false;
        }
        while let Some(c) = parser.peek_char()
            && (c == '+' || c == '-')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_inline_whitespace();
            if !Self::try_parse_factor(parser) {
                parser.recover_newline("Expected expression after operator");
            }
            parser.finish_node();
        }
        true
    }

    fn try_parse_factor(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();

        if !Self::try_parse_unary(parser) {
            return false;
        }

        while let Some(c) = parser.peek_char()
            && (c == '*' || c == '/' || c == '%')
            && parser.peek_nth_char(1) != Some('=')
        {
            parser.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
            parser.bump_char();
            parser.skip_inline_whitespace();
            if !Self::try_parse_unary(parser) {
                parser.recover_newline("Expected expression after operator");
            }
            parser.finish_node();
        }
        true
    }

    fn try_parse_unary(parser: &mut Parser) -> bool {
        if matches!(parser.peek_char(), Some('!' | '-' | '*' | '&')) {
            parser.start_node(SyntaxKind::UnaryExpression);
            parser.bump_char();
            parser.skip_inline_whitespace();
            Self::try_parse_unary(parser);
            parser.finish_node();

            return true;
        }

        Self::try_parse_postfix(parser)
    }

    fn try_parse_postfix(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();
        if !Self::try_parse_primary(parser) {
            return false;
        }

        loop {
            parser.skip_inline_whitespace();

            match parser.peek_char() {
                Some('[') => {
                    parser.start_node_at(checkpoint, SyntaxKind::IndexExpression);
                    parser.bump_char();
                    parser.skip_whitespace();
                    if !Self::try_parse_expression(parser) {
                        parser.recover_newline("Expected index expression");
                    }
                    parser.skip_whitespace();
                    parser.expect_char(']', "Expected closing bracket ']'");
                    parser.finish_node();
                }
                _ => {
                    let Some(id) = parser.peek_identifier() else {
                        break;
                    };

                    if id == "to" {
                        parser.start_node_at(checkpoint, SyntaxKind::ToCastExpression);
                        parser.bump_keyword("to");
                        parser.expect_inline_whitespace();
                        parser.expect_identifier("Expected runtime storage type");
                        parser.finish_node();
                    } else if id == "as" {
                        parser.start_node_at(checkpoint, SyntaxKind::AsCastExpression);
                        parser.bump_identifier("as");
                        parser.expect_inline_whitespace();
                        if !CSTDataType::try_parse(parser) {
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

    fn try_parse_primary(parser: &mut Parser) -> bool {
        match parser.peek_char() {
            Some('{') => CSTCompoundExpression::try_parse(parser),
            Some('[') => CSTListExpression::try_parse(parser),
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

                let should_emit_error = CSTExpression::try_parse(parser);
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

                        if !CSTExpression::try_parse(parser) {
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
            Some('\'') => {
                if let Some(text) = parser.peek_quoted_char() {
                    parser.start_node(SyntaxKind::CharExpression);
                    parser.add_token(SyntaxKind::Char, text.len());
                    parser.finish_node();

                    true
                } else {
                    false
                }
            }
            Some('"') => {
                if let Some(text) = parser.peek_quoted_string() {
                    parser.start_node(SyntaxKind::StringExpression);
                    parser.add_token(SyntaxKind::String, text.len());
                    parser.finish_node();

                    true
                } else {
                    false
                }
            }
            Some(char) if char.is_ascii_digit() => {
                let (is_fractional, text) = parser.peek_fractional_value().unwrap();

                let (has_suffix, kind) = match parser.peek_nth_char(text.len()) {
                    Some('b' | 'B') => (true, (SyntaxKind::ByteExpression)),
                    Some('s' | 'S') => (true, (SyntaxKind::ShortExpression)),
                    Some('i' | 'I') => (true, (SyntaxKind::IntegerExpression)),
                    Some('l' | 'L') => (true, (SyntaxKind::LongExpression)),
                    Some('f' | 'F') => (true, (SyntaxKind::FloatExpression)),
                    Some('d' | 'D') => (true, (SyntaxKind::DoubleExpression)),
                    _ => (
                        false,
                        if is_fractional {
                            SyntaxKind::FloatExpression
                        } else {
                            SyntaxKind::IntegerExpression
                        },
                    ),
                };

                let inner_kind = if is_fractional {
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
            _ => {
                if let Some(text) = parser.peek_identifier() {
                    match text {
                        "true" | "false" => {
                            parser.start_node(SyntaxKind::BooleanExpression);
                            parser.bump_identifier(text);
                            parser.finish_node();

                            true
                        }
                        "tellraw" => {
                            if !CSTTellrawCommandExpression::try_parse(parser) {
                                parser.start_node(SyntaxKind::VariableExpression);
                                parser.bump_identifier("tellraw");
                                parser.finish_node();
                            }

                            true
                        }
                        "score" => {
                            if !CSTScoreExpression::try_parse(parser) {
                                parser.start_node(SyntaxKind::VariableExpression);
                                parser.bump_identifier("score");
                                parser.finish_node();
                            }

                            true
                        }
                        "entity" | "block" | "storage" => {
                            if !CSTDataExpression::try_parse(parser) {
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
                                let mut is_struct_sig = true;
                                if parser.peek_char() == Some('<') {
                                    parser.bump_char();
                                    parser.skip_whitespace();
                                    if parser.peek_char() != Some('>') {
                                        loop {
                                            parser.skip_whitespace();
                                            if !CSTDataType::try_parse(parser) {
                                                is_struct_sig = false;
                                                break;
                                            }
                                            parser.skip_whitespace();
                                            if parser.peek_char() == Some(',') {
                                                parser.bump_char();
                                            } else {
                                                break;
                                            }
                                        }
                                    }
                                    if is_struct_sig {
                                        parser.skip_whitespace();
                                        if parser.peek_char() == Some('>') {
                                            parser.bump_char();
                                            parser.skip_whitespace();
                                        } else {
                                            is_struct_sig = false;
                                        }
                                    }
                                } else {
                                    parser.skip_inline_whitespace();
                                }

                                if is_struct_sig && parser.peek_char() == Some('{') {
                                    parser.bump_char();
                                    parser.skip_whitespace();
                                    while parser.peek_char() != Some('}') {
                                        parser.start_node(SyntaxKind::StructExpressionField);

                                        if !parser.expect_identifier("Expected struct field name") {
                                            CSTStructExpression::bump_until_next_field_or_end(
                                                parser,
                                            );
                                            parser.finish_node();

                                            continue;
                                        }

                                        parser.skip_whitespace();

                                        parser.expect_char(':', "Expected ':'");

                                        parser.skip_whitespace();

                                        if !Self::try_parse(parser) {
                                            parser.error("Expected expression");
                                            CSTStructExpression::bump_until_next_field_or_end(
                                                parser,
                                            );
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
                                parser.start_node_at(checkpoint, SyntaxKind::StructExpression);
                                parser.finish_node();
                            } else {
                                parser.restore_state(state);
                                parser.start_node_at(checkpoint, SyntaxKind::VariableExpression);
                                parser.finish_node();
                            }

                            true
                        }
                    }
                } else {
                    false
                }
            }
        }
    }

    #[must_use]
    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
        Some(
            (match node.kind()? {
                SyntaxKind::UnaryExpression => {
                    CSTExpressionKind::Unary(CSTUnaryExpression::cast(node)?)
                }
                SyntaxKind::VariableExpression => {
                    CSTExpressionKind::Variable(CSTVariableExpression::cast(node)?)
                }
                SyntaxKind::ByteExpression => {
                    CSTExpressionKind::Byte(CSTByteExpression::cast(node)?)
                }
                SyntaxKind::ShortExpression => {
                    CSTExpressionKind::Short(CSTShortExpression::cast(node)?)
                }
                SyntaxKind::IntegerExpression => {
                    CSTExpressionKind::Integer(CSTIntegerExpression::cast(node)?)
                }
                SyntaxKind::LongExpression => {
                    CSTExpressionKind::Long(CSTLongExpression::cast(node)?)
                }
                SyntaxKind::FloatExpression => {
                    CSTExpressionKind::Float(CSTFloatExpression::cast(node)?)
                }
                SyntaxKind::DoubleExpression => {
                    CSTExpressionKind::Double(CSTDoubleExpression::cast(node)?)
                }
                SyntaxKind::CharExpression => {
                    CSTExpressionKind::Char(CSTCharExpression::cast(node)?)
                }
                SyntaxKind::StringExpression => {
                    CSTExpressionKind::String(CSTStringExpression::cast(node)?)
                }
                SyntaxKind::AssignmentExpression => {
                    CSTExpressionKind::Assignment(CSTAssignmentExpression::cast(node)?)
                }
                SyntaxKind::BinaryExpression => {
                    CSTExpressionKind::Binary(CSTBinaryExpression::cast(node)?)
                }
                SyntaxKind::DataExpression => {
                    CSTExpressionKind::Data(CSTDataExpression::cast(node)?)
                }
                SyntaxKind::ListExpression => {
                    CSTExpressionKind::List(CSTListExpression::cast(node)?)
                }
                SyntaxKind::IndexExpression => {
                    CSTExpressionKind::Index(CSTIndexExpression::cast(node)?)
                }
                SyntaxKind::ScoreExpression => {
                    CSTExpressionKind::Score(CSTScoreExpression::cast(node)?)
                }
                SyntaxKind::TellrawCommandExpression => {
                    CSTExpressionKind::TellrawCommand(CSTTellrawCommandExpression::cast(node)?)
                }
                SyntaxKind::UnitExpression => {
                    CSTExpressionKind::Unit(CSTUnitExpression::cast(node)?)
                }
                SyntaxKind::TupleExpression => {
                    CSTExpressionKind::Tuple(CSTTupleExpression::cast(node)?)
                }
                SyntaxKind::ParenthesizedExpression => {
                    CSTExpressionKind::Paren(CSTParenthesizedExpression::cast(node)?)
                }
                SyntaxKind::AsCastExpression => {
                    CSTExpressionKind::AsCast(CSTAsCastExpression::cast(node)?)
                }
                SyntaxKind::ToCastExpression => {
                    CSTExpressionKind::ToCast(CSTToCastExpression::cast(node)?)
                }
                SyntaxKind::FieldAccessExpression => {
                    CSTExpressionKind::FieldAccess(CSTFieldAccessExpression::cast(node)?)
                }
                SyntaxKind::StructExpression => {
                    CSTExpressionKind::Struct(CSTStructExpression::cast(node)?)
                }
                _ => {
                    #[cfg(debug_assertions)]
                    println!("Failed to cast node {:?} to CSTExpression", node);

                    return None;
                }
            })
            .with_span(node.span()),
        )
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<Expression> {
        Some(
            (match self.kind {
                CSTExpressionKind::Unary(expression) => {
                    let op_kind = expression.op_kind()?;
                    let operator = match op_kind {
                        SyntaxKind::ExclamationMark => UnaryOperator::Invert,
                        SyntaxKind::Minus => UnaryOperator::Negate,
                        SyntaxKind::Star => UnaryOperator::Dereference,
                        SyntaxKind::Ampersand => UnaryOperator::Reference,
                        _ => return None,
                    };
                    let operand = expression.operand()?.lower(text)?;
                    ExpressionKind::Unary(operator, Box::new(operand))
                }
                CSTExpressionKind::Variable(expression) => {
                    let name = expression.name(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Variable(name.to_string()).with_span(self.span),
                    )
                }
                CSTExpressionKind::Byte(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Byte(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Short(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Short(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Integer(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Integer(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Long(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Long(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Float(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Float(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Double(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::Double(value).with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Char(expression) => {
                    let value = expression.value(text)?;

                    // TODO
                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::String(HighSNBTString {
                                span: self.span,
                                snbt_string: SNBTString(false, value.to_string()),
                            })
                            .with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::String(expression) => {
                    let value = expression.value(text)?;

                    ExpressionKind::Constant(
                        ConstantExpressionKind::Literal(
                            LiteralExpressionKind::String(HighSNBTString {
                                span: self.span,
                                snbt_string: SNBTString(false, value.to_string()),
                            })
                            .with_span(self.span),
                        )
                        .with_span(self.span),
                    )
                }
                CSTExpressionKind::Assignment(expression) => {
                    let left = expression.lhs()?.lower(text)?;
                    let right = expression.rhs()?.lower(text)?;
                    let op_kind = expression.op_kind()?;

                    let operator = match op_kind {
                        SyntaxKind::Equal => None,
                        SyntaxKind::PlusEqual => Some(ArithmeticOperator::Add),
                        SyntaxKind::MinusEqual => Some(ArithmeticOperator::Subtract),
                        SyntaxKind::StarEqual => Some(ArithmeticOperator::Multiply),
                        SyntaxKind::ForwardSlashEqual => Some(ArithmeticOperator::FloorDivide),
                        SyntaxKind::PercentEqual => Some(ArithmeticOperator::Modulo),
                        SyntaxKind::AmpersandEqual => Some(ArithmeticOperator::And),
                        SyntaxKind::PipeEqual => Some(ArithmeticOperator::Or),
                        SyntaxKind::LeftArrowLeftArrowEqual => Some(ArithmeticOperator::LeftShift),
                        SyntaxKind::RightArrowRightArrowEqual => {
                            Some(ArithmeticOperator::RightShift)
                        }
                        SyntaxKind::RightArrowLeftArrow => Some(ArithmeticOperator::Swap),
                        _ => return None,
                    };

                    if let Some(operator) = operator {
                        ExpressionKind::AugmentedAssignment(
                            Box::new(left),
                            operator,
                            Box::new(right),
                        )
                    } else {
                        ExpressionKind::Assignment(Box::new(left), Box::new(right))
                    }
                }
                CSTExpressionKind::Binary(expression) => {
                    let left = expression.lhs()?.lower(text)?;
                    let right = expression.rhs()?.lower(text)?;
                    let (_, op_kind) = expression.op_details(text)?;

                    match op_kind {
                        SyntaxKind::Plus
                        | SyntaxKind::Minus
                        | SyntaxKind::Star
                        | SyntaxKind::ForwardSlash
                        | SyntaxKind::Percent
                        | SyntaxKind::Ampersand
                        | SyntaxKind::Pipe
                        | SyntaxKind::LeftArrowLeftArrow
                        | SyntaxKind::RightArrowRightArrow => {
                            let operator = match op_kind {
                                SyntaxKind::Plus => ArithmeticOperator::Add,
                                SyntaxKind::Minus => ArithmeticOperator::Subtract,
                                SyntaxKind::Star => ArithmeticOperator::Multiply,
                                SyntaxKind::ForwardSlash => ArithmeticOperator::FloorDivide,
                                SyntaxKind::Percent => ArithmeticOperator::Modulo,
                                SyntaxKind::Ampersand => ArithmeticOperator::And,
                                SyntaxKind::Pipe => ArithmeticOperator::Or,
                                SyntaxKind::LeftArrowLeftArrow => ArithmeticOperator::LeftShift,
                                SyntaxKind::RightArrowRightArrow => ArithmeticOperator::RightShift,
                                _ => unreachable!(),
                            };
                            ExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
                        }
                        SyntaxKind::EqualEqual
                        | SyntaxKind::ExclamationMarkEqual
                        | SyntaxKind::RightArrow
                        | SyntaxKind::RightArrowEqual
                        | SyntaxKind::LeftArrow
                        | SyntaxKind::LeftArrowEqual => {
                            let operator = match op_kind {
                                SyntaxKind::EqualEqual => ComparisonOperator::EqualTo,
                                SyntaxKind::ExclamationMarkEqual => ComparisonOperator::NotEqualTo,
                                SyntaxKind::RightArrow => ComparisonOperator::GreaterThan,
                                SyntaxKind::RightArrowEqual => {
                                    ComparisonOperator::GreaterThanOrEqualTo
                                }
                                SyntaxKind::LeftArrow => ComparisonOperator::LessThan,
                                SyntaxKind::LeftArrowEqual => ComparisonOperator::LessThanOrEqualTo,
                                _ => unreachable!(),
                            };
                            ExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                        }
                        SyntaxKind::AmpersandAmpersand | SyntaxKind::PipePipe => {
                            let operator = match op_kind {
                                SyntaxKind::AmpersandAmpersand => LogicalOperator::And,
                                SyntaxKind::PipePipe => LogicalOperator::Or,
                                _ => unreachable!(),
                            };
                            ExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                        }
                        _ => return None,
                    }
                }
                CSTExpressionKind::Data(expression) => {
                    let target = expression.data_target()?.lower(text)?;
                    let path = expression.nbt_path()?.lower(text)?;

                    ExpressionKind::Data(target, path)
                }
                CSTExpressionKind::Score(expression) => {
                    let selector = expression.selector()?.lower(text)?;
                    let objective = expression.message(text)?.to_string();

                    ExpressionKind::PlayerScore(HighPlayerScore {
                        is_generated: false,
                        selector,
                        objective,
                    })
                }
                CSTExpressionKind::TellrawCommand(expression) => {
                    let selector = expression.selector()?.lower(text)?;
                    let value = expression.message()?.lower(text)?;

                    ExpressionKind::Command(Box::new(HighCommand::Tellraw(selector, value)))
                }
                CSTExpressionKind::Unit(_) => {
                    ExpressionKind::Constant(ConstantExpressionKind::Unit.with_span(self.span))
                }
                CSTExpressionKind::Compound(expression) => {
                    ExpressionKind::Compound(expression.lower(text))
                }
                CSTExpressionKind::Index(expression) => {
                    let target = expression.target()?.lower(text)?;
                    let index = expression.index()?.lower(text)?;

                    ExpressionKind::Index(Box::new(target), Box::new(index))
                }
                CSTExpressionKind::List(expression) => ExpressionKind::List(expression.lower(text)),
                CSTExpressionKind::Tuple(expression) => {
                    let expressions = expression
                        .expressions()
                        .into_iter()
                        .filter_map(|expression| expression.lower(text))
                        .collect();

                    ExpressionKind::Tuple(expressions)
                }
                CSTExpressionKind::Paren(expression) => {
                    return expression.expression()?.lower(text);
                }
                CSTExpressionKind::AsCast(expression) => {
                    let data_type = expression.data_type()?.lower(text)?;
                    let expression = expression.expression()?.lower(text)?;

                    ExpressionKind::AsCast(Box::new(expression), data_type)
                }
                CSTExpressionKind::ToCast(expression) => {
                    let runtime_storage_type = expression.runtime_storage_type(text)?;
                    let expression = expression.expression()?.lower(text)?;

                    let runtime_storage_type = match runtime_storage_type {
                        "data" => RuntimeStorageType::Data,
                        _ => RuntimeStorageType::Score,
                    };

                    ExpressionKind::ToCast(Box::new(expression), runtime_storage_type)
                }
                CSTExpressionKind::FieldAccess(expression) => {
                    let target = expression.target()?.lower(text)?;
                    let (field_span, field_name) = expression.field(text)?;

                    ExpressionKind::FieldAccess(
                        Box::new(target),
                        HighSNBTString {
                            span: field_span,
                            snbt_string: SNBTString(false, field_name.to_string()),
                        },
                    )
                }
                CSTExpressionKind::Struct(expression) => {
                    let (name_span, name) = expression.name(text)?;
                    let name = name.to_string();

                    let generics = expression
                        .generics()
                        .into_iter()
                        .filter_map(|data_type| data_type.lower(text))
                        .collect();

                    let fields = expression
                        .fields()
                        .filter_map(|field| {
                            let (name_span, name) = field.name(text)?;
                            let value = field.value()?.lower(text)?;

                            Some((
                                HighSNBTString {
                                    span: name_span,
                                    snbt_string: SNBTString(false, name.to_string()),
                                },
                                value,
                            ))
                        })
                        .collect();

                    ExpressionKind::Struct(name_span, name, generics, fields)
                }
            })
            .with_span(self.span),
        )
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match &self.kind {
            CSTExpressionKind::Unary(expression) => {
                if let Some(operand) = expression.operand() {
                    operand.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Variable(expression) => {
                if let Some(name_span) = expression.name_span() {
                    tokens.push(SemanticToken::new(name_span, SemanticTokenType::Variable));
                }
            }
            CSTExpressionKind::Byte(_)
            | CSTExpressionKind::Short(_)
            | CSTExpressionKind::Integer(_)
            | CSTExpressionKind::Long(_)
            | CSTExpressionKind::Float(_)
            | CSTExpressionKind::Double(_) => {
                tokens.push(SemanticToken::new(self.span, SemanticTokenType::Number));
            }
            CSTExpressionKind::Char(_) | CSTExpressionKind::String(_) => {
                tokens.push(SemanticToken::new(self.span, SemanticTokenType::String));
            }
            CSTExpressionKind::Assignment(expression) => {
                if let Some(lhs) = expression.lhs() {
                    lhs.collect_semantic_tokens(tokens);
                }

                if let Some(rhs) = expression.rhs() {
                    rhs.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Binary(expression) => {
                if let Some(lhs) = expression.lhs() {
                    lhs.collect_semantic_tokens(tokens);
                }

                if let Some(rhs) = expression.rhs() {
                    rhs.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Data(expression) => {
                if let Some(target) = expression.data_target() {
                    target.collect_semantic_tokens(tokens);
                }

                if let Some(path) = expression.nbt_path() {
                    path.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Score(expression) => {
                if let Some(selector) = expression.selector() {
                    selector.collect_semantic_tokens(tokens);
                }

                if let Some(message_span) = expression.message_span() {
                    tokens.push(SemanticToken::new(
                        message_span,
                        SemanticTokenType::Variable,
                    ));
                }
            }
            CSTExpressionKind::TellrawCommand(expression) => {
                if let Some(tellraw_keyword_span) = expression.tellraw_keyword_span() {
                    tokens.push(SemanticToken::new(
                        tellraw_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(selector) = expression.selector() {
                    selector.collect_semantic_tokens(tokens);
                }

                if let Some(message) = expression.message() {
                    message.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Unit(_) => {}
            CSTExpressionKind::Compound(expression) => {
                expression.collect_semantic_tokens(tokens);
            }
            CSTExpressionKind::List(expression) => {
                for item in expression.expressions() {
                    item.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Index(expression) => {
                if let Some(target) = expression.target() {
                    target.collect_semantic_tokens(tokens);
                }

                if let Some(index) = expression.index() {
                    index.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Tuple(expression) => {
                for item in expression.expressions() {
                    item.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::Paren(expression) => {
                if let Some(inner) = expression.expression() {
                    inner.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::AsCast(expression) => {
                if let Some(inner) = expression.expression() {
                    inner.collect_semantic_tokens(tokens);
                }

                if let Some(data_type) = expression.data_type() {
                    data_type.collect_semantic_tokens(tokens);
                }
            }
            CSTExpressionKind::ToCast(expression) => {
                if let Some(inner) = expression.expression() {
                    inner.collect_semantic_tokens(tokens);
                }

                if let Some(to_keyword_span) = expression.to_keyword_span() {
                    tokens.push(SemanticToken::new(
                        to_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(runtime_storage_type_span) = expression.runtime_storage_type_span() {
                    tokens.push(SemanticToken::new(
                        runtime_storage_type_span,
                        SemanticTokenType::Class,
                    ));
                }
            }
            CSTExpressionKind::FieldAccess(expression) => {
                if let Some(target) = expression.target() {
                    target.collect_semantic_tokens(tokens);
                }

                if let Some(token) = expression
                    .0
                    .children_tokens()
                    .filter(|t| t.kind == SyntaxKind::Identifier)
                    .last()
                {
                    tokens.push(SemanticToken::new(token.span, SemanticTokenType::Property));
                }
            }
            CSTExpressionKind::Struct(expression) => {
                if let Some(token) = expression
                    .0
                    .children_tokens()
                    .find(|t| t.kind == SyntaxKind::Identifier)
                {
                    tokens.push(SemanticToken::new(token.span, SemanticTokenType::Struct));
                }

                for generic in expression.generics() {
                    generic.collect_semantic_tokens(tokens);
                }

                for field in expression.fields() {
                    if let Some(name_token) = field
                        .0
                        .children_tokens()
                        .find(|t| t.kind == SyntaxKind::Identifier)
                    {
                        tokens.push(SemanticToken::new(
                            name_token.span,
                            SemanticTokenType::Property,
                        ));
                    }
                    if let Some(value) = field.value() {
                        value.collect_semantic_tokens(tokens);
                    }
                }
            }
        }
    }
}
