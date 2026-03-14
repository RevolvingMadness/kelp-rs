use crate::{parser::Parser, syntax::SyntaxKind};

#[must_use]
pub fn expect_whole_value(parser: &mut Parser) -> bool {
    if let Some(whole_value) = parser.peek_whole_value() {
        parser.bump_str(SyntaxKind::WholeValue, whole_value);

        true
    } else {
        parser.error("Expected whole value");

        false
    }
}
