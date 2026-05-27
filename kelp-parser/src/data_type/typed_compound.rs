use crate::{
    cst::CSTDataType, extension_traits::ParsableAstNode, parser::Parser, syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_typed_compound_data_type(parser: &mut Parser) -> bool {
    let marker = parser.mark();
    marker.start_node(parser, SyntaxKind::TypedCompoundDataType);
    parser.bump_char();

    parser.skip_whitespace();

    while parser.peek_char() != Some('}') && parser.peek_char().is_some() {
        let field_marker = parser.mark();

        field_marker.start_node(parser, SyntaxKind::TypedCompoundDataTypeField);

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

        CSTDataType::expect(parser, "Expected data type");

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
