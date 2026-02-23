use kelp_core::{
    expression::{Expression, ExpressionKind, constant::ConstantExpressionKind},
    high::{command::HighCommand, player_score::HighPlayerScore, snbt_string::HighSNBTString},
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    runtime_storage_type::RuntimeStorageType,
    span::Span,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cst_node,
    cstlib::CSTNodeType,
    lower::{
        Lowerer,
        data::{nbt_path::CSTNBTPath, target::CSTDataTarget},
        data_type::CSTDataType,
        entity_selector::CSTEntitySelector,
        expression::{
            as_cast::CSTAsCastExpression, assignment::CSTAssignmentExpression,
            compound::CSTCompoundExpression, field_access::CSTFieldAccessExpression,
            index::CSTIndexExpression, list::CSTListExpression,
            parenthesized::CSTParenthesizedExpression, r#struct::CSTStructExpression,
            to_cast::CSTToCastExpression, token::CSTTokenExpression, tuple::CSTTupleExpression,
            unary::CSTUnaryExpression, unit::CSTUnitExpression,
        },
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod as_cast;
pub mod assignment;
pub mod compound;
pub mod field_access;
pub mod index;
pub mod list;
pub mod parenthesized;
pub mod r#struct;
pub mod to_cast;
pub mod token;
pub mod tuple;
pub mod unary;
pub mod unit;

#[derive(Debug)]
pub enum CSTExpressionKind<'a> {
    Unary(CSTUnaryExpression<'a>),
    Assignment(CSTAssignmentExpression<'a>),
    Binary(CSTBinaryExpression<'a>),
    Data(CSTDataExpression<'a>),
    Score(CSTScoreExpression<'a>),
    Tellraw(CSTTellrawCommandExpression<'a>),
    Unit(CSTUnitExpression<'a>),
    Compound(CSTCompoundExpression<'a>),
    List(CSTListExpression<'a>),
    Index(CSTIndexExpression<'a>),
    Tuple(CSTTupleExpression<'a>),
    Paren(CSTParenthesizedExpression<'a>),
    ToCast(CSTToCastExpression<'a>),
    AsCast(CSTAsCastExpression<'a>),
    Token(CSTTokenExpression<'a>),
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
    pub(crate) fn is_recovery(char: char) -> bool {
        char.is_alphanumeric() || char == '('
    }

    #[must_use]
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
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
                        parser.bump_identifier("to");
                        parser.expect_inline_whitespace();
                        if let Some(text) = parser.peek_identifier() {
                            parser.add_token(SyntaxKind::RuntimeStorageType, text.len());
                        } else {
                            parser.error("Expected runtime storage type");
                        }
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

                        let _ = CSTExpression::try_parse(parser);
                        parser.skip_whitespace();
                    }
                }

                let kind = if is_tuple {
                    SyntaxKind::TupleExpression
                } else {
                    SyntaxKind::ParenthesizedExpression
                };

                parser.start_node_at(checkpoint, kind);
                parser.expect_char(')', "Expected closing parenthesis");
                parser.finish_node();

                true
            }
            Some('\'') => {
                if let Some(text) = parser.peek_quoted_char() {
                    parser.start_node(SyntaxKind::LiteralExpression);
                    parser.add_token(SyntaxKind::Char, text.len());
                    parser.finish_node();

                    true
                } else {
                    false
                }
            }
            Some('"') => {
                if let Some(text) = parser.peek_quoted_string() {
                    parser.start_node(SyntaxKind::LiteralExpression);
                    parser.add_token(SyntaxKind::String, text.len());
                    parser.finish_node();

                    true
                } else {
                    false
                }
            }
            Some(char) if char.is_ascii_digit() => {
                parser.start_node(SyntaxKind::LiteralExpression);

                let (text, kind) = if let Some(float_text) = parser.peek_float()
                    && float_text.contains('.')
                {
                    (float_text, SyntaxKind::Float)
                } else {
                    (parser.peek_integer().unwrap(), SyntaxKind::Integer)
                };

                let mut total_len = text.len();

                if let Some(suffix) = parser.peek_nth_char(total_len)
                    && "bsilfdBSILFD".contains(suffix)
                {
                    total_len += suffix.len_utf8();
                }

                parser.add_token(kind, total_len);
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
                                parser.start_node(SyntaxKind::LiteralExpression);
                                parser.bump_identifier("tellraw");
                                parser.finish_node();
                            }

                            true
                        }
                        "score" => {
                            if !CSTScoreExpression::try_parse(parser) {
                                parser.start_node(SyntaxKind::LiteralExpression);
                                parser.bump_identifier("score");
                                parser.finish_node();
                            }

                            true
                        }
                        "entity" | "block" | "storage" => {
                            if !CSTDataExpression::try_parse(parser) {
                                parser.start_node(SyntaxKind::LiteralExpression);
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
                                parser.start_node_at(checkpoint, SyntaxKind::LiteralExpression);
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

    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
        Some(
            (match node.kind()? {
                SyntaxKind::UnaryExpression => {
                    CSTExpressionKind::Unary(CSTUnaryExpression::cast(node)?)
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
                    CSTExpressionKind::Tellraw(CSTTellrawCommandExpression::cast(node)?)
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
                SyntaxKind::LiteralExpression => {
                    CSTExpressionKind::Token(CSTTokenExpression::cast(node)?)
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

    pub fn lower(self) -> Option<Expression> {
        Some(
            (match self.kind {
                CSTExpressionKind::Unary(expression) => {
                    let op_kind = expression.op_kind()?;
                    let operator = match op_kind {
                        SyntaxKind::ExclamationMark => UnaryOperator::Negate,
                        SyntaxKind::Minus => UnaryOperator::Negate,
                        SyntaxKind::Star => UnaryOperator::Dereference,
                        SyntaxKind::Ampersand => UnaryOperator::Reference,
                        _ => return None,
                    };
                    let operand = expression.operand()?.lower()?;
                    ExpressionKind::Unary(operator, Box::new(operand))
                }
                CSTExpressionKind::Assignment(expression) => {
                    let left = expression.lhs()?.lower()?;
                    let right = expression.rhs()?.lower()?;
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
                    let left = expression.lhs()?.lower()?;
                    let right = expression.rhs()?.lower()?;
                    let (_, op_kind) = expression.op_details()?;

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
                    let target = expression.data_target()?.lower()?;
                    let path = expression.nbt_path()?.lower()?;

                    ExpressionKind::Data(target, path)
                }
                CSTExpressionKind::Score(expression) => {
                    let selector = expression.selector()?.lower()?;
                    let objective = expression.message()?.to_string();

                    ExpressionKind::PlayerScore(HighPlayerScore {
                        is_generated: false,
                        selector,
                        objective,
                    })
                }
                CSTExpressionKind::Tellraw(expression) => {
                    let selector = expression.selector()?.lower()?;
                    let value = expression.message()?.lower()?;

                    ExpressionKind::Command(Box::new(HighCommand::Tellraw(selector, value)))
                }
                CSTExpressionKind::Unit(_) => {
                    ExpressionKind::Constant(ConstantExpressionKind::Unit.with_span(self.span))
                }
                CSTExpressionKind::Compound(expression) => {
                    ExpressionKind::Compound(expression.lower())
                }
                CSTExpressionKind::Index(expression) => {
                    let target = expression.target()?.lower()?;
                    let index = expression.index()?.lower()?;

                    ExpressionKind::Index(Box::new(target), Box::new(index))
                }
                CSTExpressionKind::List(expression) => ExpressionKind::List(expression.lower()),
                CSTExpressionKind::Tuple(expression) => {
                    let expressions = expression
                        .expressions()
                        .into_iter()
                        .filter_map(CSTExpression::lower)
                        .collect();

                    ExpressionKind::Tuple(expressions)
                }
                CSTExpressionKind::Paren(expression) => return expression.expression()?.lower(),
                CSTExpressionKind::AsCast(expression) => {
                    let data_type = expression.data_type()?.lower()?;
                    let expression = expression.expression()?.lower()?;

                    ExpressionKind::AsCast(Box::new(expression), data_type)
                }
                CSTExpressionKind::ToCast(expression) => {
                    let runtime_storage_type = expression.runtime_storage_type()?;
                    let expression = expression.expression()?.lower()?;

                    let runtime_storage_type = match runtime_storage_type {
                        "data" => RuntimeStorageType::Data,
                        _ => RuntimeStorageType::Score,
                    };

                    ExpressionKind::ToCast(Box::new(expression), runtime_storage_type)
                }
                CSTExpressionKind::Token(expression) => {
                    let token = expression.value_token()?;

                    let kind = token.kind;

                    match kind {
                        SyntaxKind::Integer
                        | SyntaxKind::Float
                        | SyntaxKind::String
                        | SyntaxKind::Char => {
                            let literal = Lowerer::lower_literal_from_token(token)?;
                            let span = literal.span;

                            ExpressionKind::Constant(
                                ConstantExpressionKind::Literal(literal).with_span(span),
                            )
                        }

                        SyntaxKind::Identifier => {
                            let name = token.text.to_string();
                            let span = expression.span();

                            ExpressionKind::Constant(
                                ConstantExpressionKind::Variable(name).with_span(span),
                            )
                        }

                        _ => {
                            #[cfg(debug_assertions)]
                            println!("Unexpected token kind {:?} in literal expression", kind);

                            return None;
                        }
                    }
                }
                CSTExpressionKind::FieldAccess(expression) => {
                    let target = expression.target()?.lower()?;
                    let (field_span, field_name) = expression.field()?;

                    ExpressionKind::FieldAccess(
                        Box::new(target),
                        HighSNBTString {
                            span: field_span,
                            snbt_string: SNBTString(false, field_name.to_string()),
                        },
                    )
                }
                CSTExpressionKind::Struct(expression) => {
                    let (name_span, name) = expression.name()?;
                    let name = name.to_string();

                    let generics = expression
                        .generics()
                        .into_iter()
                        .filter_map(CSTDataType::lower)
                        .collect();

                    let fields = expression
                        .fields()
                        .filter_map(|field| {
                            let (name_span, name) = field.name()?;
                            let value = field.value()?.lower()?;

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
}

cst_node!(CSTBinaryExpression, SyntaxKind::BinaryExpression);

impl<'a> CSTBinaryExpression<'a> {
    pub fn lhs(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn rhs(&self) -> Option<CSTExpression<'a>> {
        self.0.children().filter_map(CSTExpression::cast).nth(1)
    }

    pub fn op_details(&self) -> Option<(&'a str, SyntaxKind)> {
        self.0.children().find_map(|child| {
            if let CSTNodeType::Token(token) = child
                && matches!(
                    token.kind,
                    SyntaxKind::Plus
                        | SyntaxKind::Minus
                        | SyntaxKind::Star
                        | SyntaxKind::ForwardSlash
                        | SyntaxKind::Percent
                        | SyntaxKind::RightArrow
                        | SyntaxKind::RightArrowEqual
                        | SyntaxKind::LeftArrow
                        | SyntaxKind::LeftArrowEqual
                        | SyntaxKind::EqualEqual
                        | SyntaxKind::ExclamationMarkEqual
                        | SyntaxKind::AmpersandAmpersand
                        | SyntaxKind::PipePipe
                        | SyntaxKind::Ampersand
                        | SyntaxKind::Pipe
                        | SyntaxKind::LeftArrowLeftArrow
                        | SyntaxKind::RightArrowRightArrow
                )
            {
                return Some((token.text, token.kind));
            }
            None
        })
    }
}

cst_node!(CSTScoreExpression, SyntaxKind::ScoreExpression);

impl<'a> CSTScoreExpression<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::ScoreExpression);
        parser.bump_identifier("score");

        if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
            parser.restore_state(state);

            return false;
        }

        parser.expect_inline_whitespace();

        parser.expect_identifier("Expected scoreboard objective");

        parser.finish_node();

        true
    }

    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }

    pub fn message(&self) -> Option<&'a str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text)
            } else {
                None
            }
        })
    }
}

cst_node!(CSTDataExpression, SyntaxKind::DataExpression);

impl<'a> CSTDataExpression<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();

        if !CSTDataTarget::try_parse(parser) {
            return false;
        }

        parser.start_node_at(checkpoint, SyntaxKind::DataExpression);

        parser.expect_inline_whitespace();

        CSTNBTPath::try_parse(parser);

        parser.finish_node();

        true
    }
    pub fn data_target(&self) -> Option<CSTDataTarget<'a>> {
        self.0.children().find_map(CSTDataTarget::cast)
    }

    pub fn nbt_path(&self) -> Option<CSTNBTPath<'a>> {
        self.0.children().find_map(CSTNBTPath::cast)
    }
}

cst_node!(
    CSTTellrawCommandExpression,
    SyntaxKind::TellrawCommandExpression
);

impl<'a> CSTTellrawCommandExpression<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::TellrawCommandExpression);
        parser.bump_identifier("tellraw");

        let parsed_inline_whitespace = parser.expect_inline_whitespace();

        if !parsed_inline_whitespace {
            parser.restore_state(state);

            return false;
        }

        let parsed_entity_selector = CSTEntitySelector::try_parse(parser);

        if !parsed_entity_selector {
            parser.restore_state(state);

            return false;
        }

        if parsed_entity_selector {
            parser.expect_inline_whitespace();
        } else {
            parser.skip_inline_whitespace();
        }

        if !CSTExpression::try_parse(parser) {
            parser.recover_newline("Expected expression");
        }

        parser.finish_node();

        true
    }

    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }

    pub fn message(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }
}
