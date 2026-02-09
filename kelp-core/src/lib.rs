use parser_rs::{Expectation, ParseError};

pub mod command;
pub mod compile_context;
pub mod data_type;
pub mod datapack;
pub mod expression;
pub mod high;
pub mod operator;
pub mod pattern;
pub mod pattern_type;
pub mod place;
pub mod runtime_storage_type;
pub mod semantic_analysis_context;
pub mod statement;
pub mod trait_ext;

fn format_expected<T: ToString>(items: Vec<T>) -> String {
    match items.len() {
        0 => String::new(),
        1 => format!("expected {}", items[0].to_string()),
        2 => format!(
            "expected {} or {}",
            items[0].to_string(),
            items[1].to_string()
        ),
        _ => {
            let all_but_last = &items[..items.len() - 1];
            let last = &items[items.len() - 1];
            let all_but_last_str = all_but_last
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("expected {}, or {}", all_but_last_str, last.to_string())
        }
    }
}

pub fn generate_message_error(error: &ParseError) -> String {
    if !error.messages.is_empty() {
        format_expected(error.messages.to_vec())
    } else {
        let expected = error
            .expected
            .iter()
            .map(|expected| match expected {
                Expectation::Literal(literal) => format!(r#""{}""#, literal),
                Expectation::Custom(custom) => custom.to_string(),
                Expectation::Char(char) => format!("'{}'", char),
                Expectation::Digit => "digit".to_string(),
                Expectation::Identifier => "identifier".to_string(),
                Expectation::NewlineWhitespace => "whitespace containing a newline".to_string(),
                Expectation::Whitespace => "whitespace".to_string(),
                Expectation::StartOfFile => "start of file".to_string(),
                Expectation::EndOfFile => "end of file".to_string(),
            })
            .collect::<Vec<_>>();

        format_expected(expected)
    }
}
