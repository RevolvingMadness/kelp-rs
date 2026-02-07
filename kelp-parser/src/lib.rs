use crate::statement::parse_statement;
use kelp_core::statement::Statement;
use ordered_float::NotNan;
use parser_rs::{
    Expectation,
    combinators::{char, choice::choice, end_of_file, literal, take_while::take_while_one_bytes},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};
use std::num::IntErrorKind;

pub mod column_position;
pub mod command;
pub mod context;
pub mod coordinate;
pub mod data;
pub mod data_type;
pub mod dispatch;
pub mod entity_selector;
pub mod enums;
pub mod expression;
pub mod range;
pub mod resource_location;
pub mod statement;

pub fn quoted_string(input: &mut Stream) -> Option<String> {
    (|input: &mut Stream| {
        let quote_char = choice((char('"'), char('\''))).parse(input)?;

        let mut output = String::new();

        loop {
            let Some(c) = input.consume_char() else {
                return input.fail_expected(&Expectation::Char(quote_char));
            };

            if c == quote_char {
                break;
            }

            if c == '\\' {
                let Some(escaped_char) = input.consume_char() else {
                    return input.fail_message("Incomplete escape sequence");
                };

                match escaped_char {
                    'b' => output.push_str("\\b"),
                    'f' => output.push_str("\\f"),
                    'n' => output.push('\n'),
                    'r' => output.push_str("\\r"),
                    's' => output.push(' '),
                    't' => output.push_str("\\t"),
                    '\\' => output.push('\\'),
                    '\'' => output.push('\''),
                    '\"' => output.push('"'),
                    _ => {
                        return input.fail_message("Invalid escape sequence");
                    }
                }
            } else {
                output.push(c);
            }
        }

        Some(output)
    })
    .label("snbt quoted string")
    .parse(input)
}

pub fn string(input: &mut Stream) -> Option<String> {
    choice((
        quoted_string,
        identifier("unquoted string").map(|identifier| identifier.to_string()),
    ))
    .parse(input)
}

pub fn identifier<'a>(name: &'static str) -> impl FnParser<'a, Output = &'a str> {
    move |input: &mut Stream<'a>| {
        let start = input.position;

        let mut chars = input.remaining().char_indices();
        let mut end_byte_offset;

        if let Some((_, c)) = chars.next() {
            if c.is_ascii_alphabetic() || c == '_' {
                end_byte_offset = c.len_utf8();
            } else {
                let r = input.fail_expected_suggestion(&Expectation::Custom(name));

                return r;
            }
        } else {
            return input.fail_expected_suggestion(&Expectation::Custom(name));
        }

        for (i, c) in chars {
            if c.is_ascii_alphanumeric() || c == '_' {
                end_byte_offset = i + c.len_utf8();
            } else {
                break;
            }
        }

        let end_position = start + end_byte_offset;
        let slice = &input.input[start..end_position];
        input.position = end_position;

        Some(slice)
    }
}

pub fn key<'a>(name: &'static str) -> impl FnParser<'a, Output = &'a str> {
    move |input: &mut Stream<'a>| {
        let result = take_while_one_bytes(
            |c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'*',
            Expectation::Custom(name),
        )
        .separated_by::<_, ()>(char('.'))
        .sliced()
        .syntax(SemanticTokenKind::Class)
        .parse(input)?;

        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }
}

pub fn float<'a>(input: &mut Stream<'a>) -> Option<NotNan<f32>> {
    (|input: &mut Stream<'a>| {
        let start = input.position;

        char('-').optional().parse(input)?;
        digits.parse(input)?;

        (|input: &mut Stream<'a>| {
            char('.').parse(input)?;
            digits.parse(input)
        })
        .optional()
        .parse(input)?;

        let value = input.slice_from(start).parse::<f32>().unwrap();

        if value.is_infinite() {
            input.add_validation_error_span(
                start..input.position,
                "Float is greater than the maximum value of 3.40282347e+38",
            );

            Some(NotNan::new(f32::MAX).unwrap())
        } else {
            Some(NotNan::new(value).unwrap())
        }
    })
    .label("float")
    .parse(input)
}

pub fn integer(input: &mut Stream) -> Option<i32> {
    (|input: &mut Stream| {
        let start = input.position;

        let is_negative = char('-').optional().parse(input)?.is_some();

        let bytes = input.remaining().as_bytes();
        let (radix, check_digit): (u32, fn(u8) -> bool) =
            if bytes.starts_with(b"0x") || bytes.starts_with(b"0X") {
                input.position += 2;
                (16, |c| c.is_ascii_hexdigit())
            } else if bytes.starts_with(b"0b") || bytes.starts_with(b"0B") {
                input.position += 2;
                (2, |c| c == b'0' || c == b'1')
            } else if bytes.starts_with(b"0o") || bytes.starts_with(b"0O") {
                input.position += 2;
                (8, |c| (b'0'..=b'7').contains(&c))
            } else {
                (10, |c| c.is_ascii_digit())
            };

        let digits_start = input.position;

        take_while_one_bytes(check_digit, Expectation::Digit).parse(input)?;

        let digits_slice = input.slice_from(digits_start);

        let result = i64::from_str_radix(digits_slice, radix);

        match result {
            Ok(mut value) => {
                if is_negative {
                    value = -value;
                }

                if value > i32::MAX as i64 {
                    input.add_validation_error_span(
                        start..input.position,
                        "Integer is greater than the maximum value of 2147483647",
                    );
                    Some(i32::MAX)
                } else if value < i32::MIN as i64 {
                    input.add_validation_error_span(
                        start..input.position,
                        "Integer is greater than the minimum value of -2147483648",
                    );
                    Some(i32::MIN)
                } else {
                    Some(value as i32)
                }
            }
            Err(error) => match error.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                    if is_negative {
                        input.add_validation_error_span(
                            start..input.position,
                            "Integer is greater than the minimum value of -2147483648",
                        );
                        Some(i32::MIN)
                    } else {
                        input.add_validation_error_span(
                            start..input.position,
                            "Integer is greater than the maximum value of 2147483647",
                        );
                        Some(i32::MAX)
                    }
                }
                IntErrorKind::Empty | IntErrorKind::InvalidDigit | IntErrorKind::Zero => {
                    unreachable!()
                }
                _ => unreachable!(),
            },
        }
    })
    .syntax(SemanticTokenKind::Number)
    .label("integer")
    .parse(input)
}

pub fn boolean(input: &mut Stream) -> Option<bool> {
    choice((literal("true").map_to(true), literal("false").map_to(false)))
        .label("snbt boolean")
        .parse(input)
}

pub fn whitespace(input: &mut Stream) -> Option<()> {
    loop {
        let bytes = input.remaining().as_bytes();

        let ws_len = bytes
            .iter()
            .position(|b| !b.is_ascii_whitespace())
            .unwrap_or(bytes.len());
        if ws_len > 0 {
            input.allow_suggestions(input.position, input.position + ws_len);
        }
        input.position += ws_len;

        let bytes = input.remaining().as_bytes();

        if bytes.starts_with(b"//") {
            let len = bytes
                .iter()
                .position(|b| *b == b'\n')
                .map(|p| p + 1)
                .unwrap_or(bytes.len());
            input.position += len;
        } else if bytes.starts_with(b"/*") {
            match bytes.windows(2).position(|w| w == b"*/") {
                Some(pos) => {
                    input.position += pos + 2;
                }
                None => {
                    input.position += bytes.len();

                    input.add_suggestion(input.position, &Expectation::Literal("*/"));

                    return input.fail_message("Unclosed block comment");
                }
            }
        } else {
            break;
        }
    }

    Some(())
}

pub fn required_whitespace(input: &mut Stream) -> Option<()> {
    let start_pos = input.position;
    let mut is_first = true;
    loop {
        let bytes = input.remaining().as_bytes();

        let ws_len = bytes
            .iter()
            .position(|b| !b.is_ascii_whitespace())
            .unwrap_or(bytes.len());
        let threshold = if is_first { 1 } else { 0 };
        if ws_len > threshold {
            input.allow_suggestions(input.position + threshold, input.position + ws_len);
        }
        input.position += ws_len;

        let bytes = input.remaining().as_bytes();

        if bytes.starts_with(b"//") {
            let len = bytes
                .iter()
                .position(|b| *b == b'\n')
                .map(|p| p + 1)
                .unwrap_or(bytes.len());
            input.position += len;
        } else if bytes.starts_with(b"/*") {
            match bytes.windows(2).position(|w| w == b"*/") {
                Some(pos) => {
                    input.position += pos + 2;
                }
                None => {
                    input.position += bytes.len();

                    input.add_suggestion(input.position, &Expectation::Literal("*/"));

                    return input.fail_message("Unclosed block comment");
                }
            }
        } else {
            break;
        }

        is_first = false;
    }

    if input.position == start_pos {
        if let Some(force_suggest_range) = input.force_suggest_range
            && force_suggest_range.contains(input.position)
        {
            Some(())
        } else {
            input.fail_expected(&Expectation::Whitespace)
        }
    } else {
        Some(())
    }
}

pub fn inline_whitespace(input: &mut Stream) -> Option<()> {
    loop {
        let bytes = input.remaining().as_bytes();

        let ws_len = bytes
            .iter()
            .position(|b| !b.is_ascii_whitespace() || *b == b'\n')
            .unwrap_or(bytes.len());
        if ws_len > 0 {
            input.allow_suggestions(input.position, input.position + ws_len);
        }
        input.position += ws_len;

        let bytes = input.remaining().as_bytes();

        if bytes.starts_with(b"//") {
            let len = bytes
                .iter()
                .position(|b| *b == b'\n')
                .unwrap_or(bytes.len());
            input.position += len;
            break;
        } else if bytes.starts_with(b"/*") {
            match bytes.windows(2).position(|w| w == b"*/") {
                Some(pos) => {
                    input.position += pos + 2;
                }
                None => {
                    input.position += bytes.len();

                    input.add_suggestion(input.position, &Expectation::Literal("*/"));

                    return input.fail_message("Unclosed block comment");
                }
            }
        } else {
            break;
        }
    }
    Some(())
}

pub fn required_inline_whitespace(input: &mut Stream) -> Option<()> {
    let start_pos = input.position;
    let mut is_first = true;

    loop {
        let bytes = input.remaining().as_bytes();

        let ws_len = bytes
            .iter()
            .position(|b| !b.is_ascii_whitespace() || *b == b'\r' || *b == b'\n')
            .unwrap_or(bytes.len());

        let threshold = if is_first { 1 } else { 0 };
        if ws_len > threshold {
            input.allow_suggestions(input.position + threshold, input.position + ws_len);
        }
        input.position += ws_len;

        let bytes = input.remaining().as_bytes();

        if bytes.starts_with(b"//") {
            let len = bytes
                .iter()
                .position(|b| *b == b'\n')
                .unwrap_or(bytes.len());
            input.position += len;
            break;
        } else if bytes.starts_with(b"/*") {
            match bytes.windows(2).position(|w| w == b"*/") {
                Some(pos) => {
                    input.position += pos + 2;
                }
                None => {
                    input.position += bytes.len();

                    input.add_suggestion(input.position, &Expectation::Literal("*/"));

                    return input.fail_message("Unclosed block comment");
                }
            }
        } else {
            break;
        }

        is_first = false;
    }

    if input.position == start_pos {
        if let Some(force_suggest_range) = input.force_suggest_range
            && force_suggest_range.contains(input.position)
        {
            Some(())
        } else {
            input.fail_expected(&Expectation::Whitespace)
        }
    } else {
        Some(())
    }
}

pub fn newline_whitespace<'a>(name: &'static str) -> impl FnParser<'a, Output = ()> {
    move |input: &mut Stream| {
        let mut contains_newline = false;

        loop {
            let bytes = input.remaining().as_bytes();

            let ws_len = bytes
                .iter()
                .position(|b| !b.is_ascii_whitespace())
                .unwrap_or(bytes.len());

            if ws_len > 0 {
                if let Some(nl_pos) = bytes[..ws_len].iter().position(|b| *b == b'\n') {
                    contains_newline = true;

                    let start = input.position + nl_pos + 1;
                    let end = input.position + ws_len;

                    input.allow_suggestions(start, end);
                }
                input.position += ws_len;
            }

            let bytes = input.remaining().as_bytes();

            if bytes.starts_with(b"//") {
                let len = bytes
                    .iter()
                    .position(|b| *b == b'\n')
                    .map(|p| p + 1)
                    .unwrap_or(bytes.len());

                if len > 0 && bytes.get(len - 1) == Some(&b'\n') {
                    contains_newline = true;
                }
                input.position += len;
                input.disallow_suggestions();
            } else if bytes.starts_with(b"/*") {
                match bytes.windows(2).position(|w| w == b"*/") {
                    Some(pos) => {
                        if bytes[..pos + 2].contains(&b'\n') {
                            contains_newline = true;
                        }
                        input.position += pos + 2;
                    }
                    None => {
                        input.position += bytes.len();

                        input.add_suggestion(input.position, &Expectation::Literal("*/"));

                        return input.fail_message("Unclosed block comment");
                    }
                }
            } else {
                break;
            }
        }

        if contains_newline {
            Some(())
        } else {
            input.fail_expected(&Expectation::Custom(name))
        }
    }
}

pub fn digits(input: &mut Stream) -> Option<()> {
    let bytes = input.remaining().as_bytes();

    let digits_len = bytes
        .iter()
        .position(|b| !b.is_ascii_digit())
        .unwrap_or(bytes.len());

    if digits_len == 0 {
        input.fail_expected(&Expectation::Digit)
    } else {
        input.position += digits_len;

        Some(())
    }
}

pub fn file(input: &mut Stream) -> Option<Vec<Statement>> {
    whitespace.parse(input)?;

    let result = parse_statement
        .separated_by(newline_whitespace("end of statement"))
        .parse(input)?;

    whitespace.parse(input)?;

    end_of_file.parse(input)?;

    Some(result)
}
