#[macro_export]
macro_rules! create_dispatch_parser {
    (
        $vis:vis fn $fn_name:ident -> $return_type:ty,
        $type_name:literal,
        $default_value:expr,
        identifier,
        { $( ($primary_key:expr $(, $alias:expr)*) => $value:expr ),* $(,)? }
    ) => {
        $vis fn $fn_name(input: &mut parser_rs::stream::Stream) -> Option<$return_type> {
            $(
                input.suggest_literal(input.position, $primary_key);
            )*

            let (span, parsed_key) = parser_rs::fn_parser::FnParser::parse(&mut parser_rs::fn_parser::FnParser::spanned($crate::identifier($type_name)), input)?;

            match parsed_key {
                $(
                    $primary_key => Some($value),
                    $(
                        $alias => Some($value),
                    )*
                )*
                _ => {
                    input.add_validation_error_span(span.clone(), concat!("Unknown ", $type_name));

                    Some($default_value)
                }
            }
        }
    };
    (
        $vis:vis fn $fn_name:ident -> $return_type:ty,
        $type_name:literal,
        $default_value:expr,
        char,
        { $( ($primary_key:expr $(, $alias:expr)*) => $value:expr ),* $(,)? }
    ) => {
        $vis fn $fn_name(input: &mut parser_rs::Stream) -> Option<$return_type> {
            use parser_rs::{Expectation, FnParser};

            let start = input.position;
            if let Some(parsed_char) = input.current_char() {
                match parsed_char {
                    $(
                        $primary_key => {
                            input.position += parsed_char.len_utf8();
                            Some($value)
                        },
                        $(
                            $alias => {
                                input.position += parsed_char.len_utf8();
                                Some($value)
                            },
                        )*
                    )*
                    _ => {
                        input.position += parsed_char.len_utf8();
                        let span = start..input.position;
                        input.add_validation_error_span(span, concat!("Invalid ", $type_name));

                        Some($default_value)
                    }
                }
            } else {
                input.fail_expected(&Expectation::Custom($type_name))
            }
        }
    };
}
