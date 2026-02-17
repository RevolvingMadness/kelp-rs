use std::{collections::BTreeMap, num::IntErrorKind};

use kelp_core::{
    data_type::high::HighDataType,
    expression::{
        Expression, ExpressionCompoundKind, ExpressionKind,
        constant::{ConstantExpression, ConstantExpressionKind},
        literal::{LiteralExpression, LiteralExpressionKind},
    },
    high::snbt_string::HighSNBTString,
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    runtime_storage_type::RuntimeStorageType,
};
use minecraft_command_types::snbt::SNBTString;
use ordered_float::NotNan;
use parser_rs::{
    Expectation,
    combinators::{char, choice::choice, literal},
    fn_parser::FnParser,
    parser_range::ParserRange,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

use crate::{
    command::{
        data::{parse_data_target, parse_nbt_path},
        execute::parse_player_score,
        parse_command,
    },
    data_type::parse_data_type,
    digits, identifier, inline_whitespace, integer, quoted_string, required_inline_whitespace,
    string, whitespace,
};

#[inline]
pub fn expression(input: &mut Stream) -> Option<Expression> {
    let has_tick = char('`').optional().parse(input)?.is_some();

    let result = assignment(input);

    if has_tick {
        char('`').parse(input)?;
    }

    result
}

fn assignment(input: &mut Stream) -> Option<Expression> {
    let left = logical_or(input)?;

    if let Some(operator) = choice((
        literal("+=").map_to(ArithmeticOperator::Add),
        literal("-=").map_to(ArithmeticOperator::Subtract),
        literal("*=").map_to(ArithmeticOperator::Multiply),
        literal("/=").map_to(ArithmeticOperator::FloorDivide),
        literal("%=").map_to(ArithmeticOperator::Modulo),
        literal("&=").map_to(ArithmeticOperator::And),
        literal("|=").map_to(ArithmeticOperator::Or),
        literal("<<=").map_to(ArithmeticOperator::LeftShift),
        literal(">>=").map_to(ArithmeticOperator::RightShift),
        literal("><").map_to(ArithmeticOperator::Swap),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;

        let right = expression(input)?;
        Some(Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::AugmentedAssignment(Box::new(left), operator, Box::new(right)),
        })
    } else if char('=').optional().parse(input)?.is_some() {
        inline_whitespace(input)?;

        let right = expression(input)?;

        Some(Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::Assignment(Box::new(left), Box::new(right)),
        })
    } else {
        Some(left)
    }
}

fn logical_or(input: &mut Stream) -> Option<Expression> {
    let mut left = logical_and(input)?;

    while literal("||").optional().parse(input)?.is_some() {
        inline_whitespace(input)?;
        let right = logical_and(input)?;
        let end = right.span.end;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end,
            },
            kind: ExpressionKind::Logical(Box::new(left), LogicalOperator::Or, Box::new(right)),
        };
    }

    Some(left)
}

fn logical_and(input: &mut Stream) -> Option<Expression> {
    let mut left = bitwise_or(input)?;

    while literal("&&").optional().parse(input)?.is_some() {
        inline_whitespace(input)?;
        let right = bitwise_or(input)?;
        let end = right.span.end;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end,
            },
            kind: ExpressionKind::Logical(Box::new(left), LogicalOperator::And, Box::new(right)),
        };
    }

    Some(left)
}

fn bitwise_or(input: &mut Stream) -> Option<Expression> {
    let mut left = bitwise_and(input)?;

    while char('|')
        .not_followed_by(char('|'))
        .optional()
        .parse(input)?
        .is_some()
    {
        inline_whitespace(input)?;
        let right = bitwise_and(input)?;
        let end = right.span.end;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end,
            },
            kind: ExpressionKind::Arithmetic(
                Box::new(left),
                ArithmeticOperator::Or,
                Box::new(right),
            ),
        };
    }

    Some(left)
}

fn bitwise_and(input: &mut Stream) -> Option<Expression> {
    let mut left = comparison(input)?;

    while char('&')
        .not_followed_by(char('&'))
        .optional()
        .parse(input)?
        .is_some()
    {
        inline_whitespace(input)?;
        let right = comparison(input)?;
        let end = right.span.end;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end,
            },
            kind: ExpressionKind::Arithmetic(
                Box::new(left),
                ArithmeticOperator::And,
                Box::new(right),
            ),
        };
    }

    Some(left)
}

fn comparison(input: &mut Stream) -> Option<Expression> {
    let mut left = shift(input)?;

    while let Some(operator) = choice((
        literal("==").map_to(ComparisonOperator::EqualTo),
        literal("!=").map_to(ComparisonOperator::NotEqualTo),
        literal("<=").map_to(ComparisonOperator::LessThanOrEqualTo),
        literal(">=").map_to(ComparisonOperator::GreaterThanOrEqualTo),
        char('<').map_to(ComparisonOperator::LessThan),
        char('>')
            .not_followed_by(char('<'))
            .map_to(ComparisonOperator::GreaterThan),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;

        let right = shift(input)?;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::Comparison(Box::new(left), operator, Box::new(right)),
        };
    }

    Some(left)
}

fn shift(input: &mut Stream) -> Option<Expression> {
    let mut left = term(input)?;

    while let Some(operator) = choice((
        literal("<<").map_to(ArithmeticOperator::LeftShift),
        literal(">>").map_to(ArithmeticOperator::RightShift),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;
        let right = term(input)?;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right)),
        };
    }

    Some(left)
}

fn term(input: &mut Stream) -> Option<Expression> {
    let mut left = factor(input)?;

    while let Some(operator) = choice((
        char('+')
            .not_followed_by(char('='))
            .map_to(ArithmeticOperator::Add),
        char('-')
            .not_followed_by(char('='))
            .map_to(ArithmeticOperator::Subtract),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;

        let right = factor(input)?;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right)),
        };
    }

    Some(left)
}

fn factor(input: &mut Stream) -> Option<Expression> {
    let mut left = cast(input)?;

    while let Some(operator) = choice((
        char('*')
            .not_followed_by(char('='))
            .map_to(ArithmeticOperator::Multiply),
        char('/')
            .not_followed_by(char('='))
            .map_to(ArithmeticOperator::FloorDivide),
        char('%')
            .not_followed_by(char('='))
            .map_to(ArithmeticOperator::Modulo),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;

        let right = cast(input)?;

        left = Expression {
            span: ParserRange {
                start: left.span.start,
                end: right.span.end,
            },
            kind: ExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right)),
        };
    }

    Some(left)
}

fn cast(input: &mut Stream) -> Option<Expression> {
    let mut left = unary(input)?;

    loop {
        inline_whitespace(input)?;

        if literal("as").optional().parse(input)?.is_some() {
            required_inline_whitespace(input)?;

            let type_def = parse_data_type(input)?;
            let end = input.position;

            left = Expression {
                span: ParserRange {
                    start: left.span.start,
                    end,
                },
                kind: ExpressionKind::AsCast(Box::new(left), type_def),
            };
        } else if literal("to").optional().parse(input)?.is_some() {
            required_inline_whitespace(input)?;

            let (runtime_storage_type_span, runtime_storage_type) =
                identifier("runtime storage type").spanned().parse(input)?;

            let runtime_storage_type = match runtime_storage_type {
                "score" => RuntimeStorageType::Score,
                "data" => RuntimeStorageType::Data,
                _ => {
                    input.add_validation_error_span(
                        runtime_storage_type_span,
                        format!("Unknown runtime storage type '{}'", runtime_storage_type),
                    );
                    continue;
                }
            };

            left = Expression {
                span: ParserRange {
                    start: left.span.start,
                    end: input.position,
                },
                kind: ExpressionKind::ToCast(Box::new(left), runtime_storage_type),
            };
        } else {
            break;
        }
    }

    Some(left)
}

fn unary(input: &mut Stream) -> Option<Expression> {
    let start = input.position;

    if let Some(operator) = choice((
        char('-').map_to(UnaryOperator::Negate),
        char('&').map_to(UnaryOperator::Reference),
        char('*').map_to(UnaryOperator::Dereference),
        char('!').map_to(UnaryOperator::Invert),
    ))
    .optional()
    .parse(input)?
    {
        inline_whitespace(input)?;

        let expr = unary(input)?;

        Some(Expression {
            span: ParserRange {
                start,
                end: expr.span.end,
            },
            kind: ExpressionKind::Unary(operator, Box::new(expr)),
        })
    } else {
        postfix(input)
    }
}

fn postfix(input: &mut Stream) -> Option<Expression> {
    let mut left = atom(input)?;

    loop {
        if char('[').optional().parse(input)?.is_some() {
            inline_whitespace(input)?;

            let index = expression(input)?;

            inline_whitespace(input)?;

            let (end_span, _) = char(']').spanned().parse(input)?;

            left = Expression {
                span: ParserRange {
                    start: left.span.start,
                    end: end_span.end,
                },
                kind: ExpressionKind::Index(Box::new(left), Box::new(index)),
            };

            inline_whitespace(input)?;
        } else if char('.').optional().parse(input)?.is_some() {
            inline_whitespace(input)?;

            let (field_span, field) = choice((string, integer.map(|number| number.to_string())))
                .spanned()
                .syntax(SemanticTokenKind::Variable)
                .parse(input)?;

            left = Expression {
                span: ParserRange {
                    start: left.span.start,
                    end: input.position,
                },
                kind: ExpressionKind::FieldAccess(
                    Box::new(left),
                    HighSNBTString {
                        span: field_span,
                        snbt_string: SNBTString(false, field),
                    },
                ),
            };

            inline_whitespace(input)?;
        } else {
            break;
        }
    }

    Some(left)
}

pub fn parse_compound(input: &mut Stream) -> Option<ExpressionCompoundKind> {
    char('{').parse(input)?;
    whitespace(input)?;
    let elements: Vec<(HighSNBTString, Expression)> = (|input: &mut Stream| {
        whitespace(input)?;
        let (key_span, key) = string
            .spanned()
            .syntax(SemanticTokenKind::Variable)
            .parse(input)?;
        whitespace(input)?;
        char(':').parse(input)?;
        whitespace(input)?;
        let value = expression(input)?;
        whitespace(input)?;

        Some((
            HighSNBTString {
                span: key_span,
                snbt_string: SNBTString(false, key),
            },
            value,
        ))
    })
    .separated_by(char(','))
    .parse(input)?;
    char('}').parse(input)?;
    Some(elements.into_iter().collect::<BTreeMap<_, _>>())
}

pub fn literal_expression(input: &mut Stream) -> Option<LiteralExpressionKind> {
    choice((
        numeric_parser,
        choice((literal("true").map_to(true), literal("false").map_to(false)))
            .syntax(SemanticTokenKind::Keyword)
            .map(LiteralExpressionKind::Boolean),
        quoted_string.spanned().map(|(string_span, string)| {
            LiteralExpressionKind::String(HighSNBTString {
                span: string_span,
                snbt_string: SNBTString(false, string),
            })
        }),
    ))
    .parse(input)
}

pub fn atom(input: &mut Stream) -> Option<Expression> {
    let (left_span, left_kind) = choice((
        literal_expression.spanned().map(|(kind_span, kind)| {
            ExpressionKind::Constant(ConstantExpression {
                span: kind_span,
                kind: ConstantExpressionKind::Literal(LiteralExpression {
                    span: kind_span,
                    kind,
                }),
            })
        }),
        char('_')
            .syntax(SemanticTokenKind::Variable)
            .spanned()
            .map(|(span, _)| {
                ExpressionKind::Constant(ConstantExpression {
                    span,
                    kind: ConstantExpressionKind::Underscore,
                })
            }),
        |input: &mut Stream| {
            char('[').parse(input)?;

            let elements = (|input: &mut Stream| {
                whitespace(input)?;
                let element = expression(input)?;
                whitespace(input)?;

                Some(element)
            })
            .separated_by(char(','))
            .parse(input)?;

            char(']').parse(input)?;

            Some(ExpressionKind::List(elements))
        },
        parse_compound.map(ExpressionKind::Compound),
        |input: &mut Stream| {
            literal("score")
                .syntax(SemanticTokenKind::Class)
                .parse(input)?;
            required_inline_whitespace(input)?;
            parse_player_score
                .parse(input)
                .map(ExpressionKind::PlayerScore)
        },
        |input: &mut Stream| {
            let target = parse_data_target(false).parse(input)?;
            required_inline_whitespace(input)?;
            let path = parse_nbt_path(input)?;
            Some(ExpressionKind::Data(target, path))
        },
        |input: &mut Stream| {
            let start = input.position;

            char('(').parse(input)?;

            let (elements, has_trailing_comma) = (|input: &mut Stream| {
                whitespace(input)?;
                let element = expression(input)?;
                whitespace(input)?;
                Some(element)
            })
            .separated_by_trailing::<_, Vec<_>>(char(','))
            .parse(input)?;
            char(')').parse(input)?;

            if !has_trailing_comma && elements.len() == 1 {
                Some(elements.into_iter().next().unwrap().kind)
            } else if !has_trailing_comma && elements.is_empty() {
                Some(ExpressionKind::Constant(ConstantExpression {
                    span: ParserRange {
                        start,
                        end: input.position,
                    },
                    kind: ConstantExpressionKind::Unit,
                }))
            } else {
                Some(ExpressionKind::Tuple(elements))
            }
        },
        |input: &mut Stream| {
            let (name_span, name) = identifier("struct or variable name")
                .spanned()
                .parse(input)?;

            let generics_fields = (|input: &mut Stream| {
                let generics: Option<Vec<HighDataType>> = (|input: &mut Stream| {
                    char('<').parse(input)?;
                    let generics = parse_data_type
                        .padded(whitespace)
                        .syntax(SemanticTokenKind::TypeParameter)
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

                let fields = (|input: &mut Stream| {
                    char('{').parse(input)?;
                    whitespace(input)?;

                    let fields = (|input: &mut Stream| {
                        let (field_name_span, field_name) = identifier("field name")
                            .spanned()
                            .syntax(SemanticTokenKind::Variable)
                            .parse(input)?;
                        whitespace(input)?;
                        char(':').parse(input)?;
                        whitespace(input)?;
                        let value = expression(input)?;

                        Some((
                            HighSNBTString {
                                span: field_name_span,
                                snbt_string: SNBTString(false, field_name.to_string()),
                            },
                            value,
                        ))
                    })
                    .padded(whitespace)
                    .separated_by::<_, Vec<_>>(char(','))
                    .parse(input)?;

                    char('}').parse(input)?;

                    Some(fields)
                })
                .parse(input)?;

                Some((generics, fields))
            })
            .optional()
            .parse(input)?;

            Some(if let Some((generics, fields)) = generics_fields {
                input.add_syntax(name_span, SemanticTokenKind::Struct);

                ExpressionKind::Struct(
                    name_span,
                    name.to_string(),
                    generics.unwrap_or_default(),
                    fields.into_iter().collect(),
                )
            } else {
                input.add_syntax(name_span, SemanticTokenKind::Variable);

                ExpressionKind::Constant(ConstantExpression {
                    span: name_span,
                    kind: ConstantExpressionKind::Variable(name.to_string()),
                })
            })
        },
        |input: &mut Stream<'_>| {
            char('/').optional().parse(input)?;

            parse_command
                .map(|command| ExpressionKind::Command(Box::new(command)))
                .parse(input)
        },
    ))
    .spanned()
    .parse(input)?;

    inline_whitespace(input)?;

    Some(Expression {
        span: left_span,
        kind: left_kind,
    })
}

pub fn binary_digits(input: &mut Stream) -> Option<()> {
    let bytes = input.remaining().as_bytes();
    let len = bytes
        .iter()
        .position(|b| !matches!(b, b'0' | b'1'))
        .unwrap_or(bytes.len());

    if len == 0 {
        input.fail_expected(&Expectation::Digit)
    } else {
        input.position += len;
        Some(())
    }
}

pub fn octal_digits(input: &mut Stream) -> Option<()> {
    let bytes = input.remaining().as_bytes();
    let len = bytes
        .iter()
        .position(|b| !matches!(b, b'0'..=b'7'))
        .unwrap_or(bytes.len());

    if len == 0 {
        input.fail_expected(&Expectation::Digit)
    } else {
        input.position += len;
        Some(())
    }
}

pub fn hex_digits(input: &mut Stream) -> Option<()> {
    let bytes = input.remaining().as_bytes();
    let len = bytes
        .iter()
        .position(|b| !b.is_ascii_hexdigit())
        .unwrap_or(bytes.len());

    if len == 0 {
        input.fail_expected(&Expectation::Digit)
    } else {
        input.position += len;
        Some(())
    }
}

pub fn numeric_parser<'a>(input: &mut Stream<'a>) -> Option<LiteralExpressionKind> {
    (|input: &mut Stream<'a>| {
        let (all_span, (all_slice, (digits_slice, radix, is_float))) =
            (|input: &mut Stream<'a>| {
                let bytes = input.remaining().as_bytes();

                if bytes.starts_with(b"0x") || bytes.starts_with(b"0X") {
                    input.position += 2;
                    let slice = hex_digits.sliced().parse(input)?;
                    Some((slice, 16, false))
                } else if bytes.starts_with(b"0b") || bytes.starts_with(b"0B") {
                    input.position += 2;
                    let slice = binary_digits.sliced().parse(input)?;
                    Some((slice, 2, false))
                } else if bytes.starts_with(b"0o") || bytes.starts_with(b"0O") {
                    input.position += 2;
                    let slice = octal_digits.sliced().parse(input)?;
                    Some((slice, 8, false))
                } else {
                    let whole_slice = digits.sliced().parse(input)?;

                    Some((
                        whole_slice,
                        10,
                        if char('.').optional().parse(input)?.is_some() {
                            digits(input)?;
                            true
                        } else {
                            false
                        },
                    ))
                }
            })
            .sliced_include()
            .spanned()
            .parse(input)?;

        #[derive(Clone)]
        enum NumericKind {
            Byte,
            Short,
            Integer,
            Long,
            Float,
            Double,
        }

        impl NumericKind {
            fn is_float(&self) -> bool {
                matches!(self, NumericKind::Float | NumericKind::Double)
            }
        }

        let suffix = choice((
            choice((char('B'), char('b'))).map_to(NumericKind::Byte),
            choice((char('S'), char('s'))).map_to(NumericKind::Short),
            choice((char('I'), char('i'))).map_to(NumericKind::Integer),
            choice((char('L'), char('l'))).map_to(NumericKind::Long),
            choice((char('F'), char('f'))).map_to(NumericKind::Float),
            choice((char('D'), char('d'))).map_to(NumericKind::Double),
        ))
        .optional()
        .parse(input)?;

        if let Some(suffix) = &suffix
            && !suffix.is_float()
            && is_float
        {
            input.add_validation_error_span(all_span, "Type suffix is not a float type");
        }

        macro_rules! parse_int {
            ($type:ty, $variant:expr, $name:literal, $max:expr, $min:expr) => {{
                let result = if is_float {
                    input.add_validation_error_span(
                        all_span,
                        concat!($name, " is not a float type"),
                    );
                    <$type>::from_str_radix(digits_slice, radix)
                } else {
                    <$type>::from_str_radix(digits_slice, radix)
                };

                $variant(match result {
                    Ok(value) => value,
                    Err(error) => match error.kind() {
                        IntErrorKind::PosOverflow => {
                            input.add_validation_error_span(
                                all_span,
                                format!(
                                    concat!($name, " is greater than the maximum value of {}"),
                                    $max
                                ),
                            );
                            $max
                        }
                        IntErrorKind::NegOverflow => {
                            input.add_validation_error_span(
                                all_span,
                                format!(
                                    concat!($name, " is less than the minimum value of {}"),
                                    $min
                                ),
                            );
                            $min
                        }
                        _ => {
                            input.add_validation_error_span(all_span, "Invalid numeric literal");
                            0
                        }
                    },
                })
            }};
        }

        Some(match (suffix, is_float) {
            (Some(NumericKind::Byte), _) => {
                parse_int!(i8, LiteralExpressionKind::Byte, "Byte", i8::MAX, i8::MIN)
            }
            (Some(NumericKind::Short), _) => {
                parse_int!(
                    i16,
                    LiteralExpressionKind::Short,
                    "Short",
                    i16::MAX,
                    i16::MIN
                )
            }
            (Some(NumericKind::Integer), _) | (None, false) => {
                parse_int!(
                    i32,
                    LiteralExpressionKind::Integer,
                    "Integer",
                    i32::MAX,
                    i32::MIN
                )
            }
            (Some(NumericKind::Long), _) => {
                parse_int!(i64, LiteralExpressionKind::Long, "Long", i64::MAX, i64::MIN)
            }
            (Some(NumericKind::Float), _) | (None, true) => {
                if radix == 10 {
                    LiteralExpressionKind::Float(all_slice.parse().unwrap())
                } else {
                    let val = i64::from_str_radix(digits_slice, radix).unwrap_or(0);
                    LiteralExpressionKind::Float(NotNan::new(val as f32).unwrap())
                }
            }
            (Some(NumericKind::Double), _) => {
                if radix == 10 {
                    LiteralExpressionKind::Double(all_slice.parse().unwrap())
                } else {
                    let val = i64::from_str_radix(digits_slice, radix).unwrap_or(0);
                    LiteralExpressionKind::Double(NotNan::new(val as f64).unwrap())
                }
            }
        })
    })
    .syntax(SemanticTokenKind::Number)
    .parse(input)
}
