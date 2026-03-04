use crate::{lower::data_type::try_parse_data_type, parser::Parser, syntax::SyntaxKind};

#[must_use]
pub fn try_parse_typed_compound_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.start_node_at(checkpoint, SyntaxKind::TypedCompoundDataType);

    parser.bump_char();
    parser.skip_whitespace();

    while parser.peek_char() != Some('}') && parser.peek_char().is_some() {
        let field_checkpoint = parser.checkpoint();
        parser.start_node_at(field_checkpoint, SyntaxKind::TypedCompoundDataTypeField);

        if let Some(text) = parser.peek_identifier() {
            parser.add_token(SyntaxKind::TypedCompoundDataTypeFieldName, text.len());
        } else {
            parser.error("Expected field name");
            parser.finish_node();
            break;
        }

        parser.skip_whitespace();
        parser.expect_char(':', "Expected ':' after field name");
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected data type");
        }

        parser.skip_whitespace();
        parser.finish_node();

        if parser.try_bump_char(',') {
            parser.skip_whitespace();
        } else {
            break;
        }
    }

    parser.expect_char('}', "Expected closing brace '}'");
    parser.finish_node();
    true
}
