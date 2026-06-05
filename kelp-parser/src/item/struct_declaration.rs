use crate::{
    cst::{CSTGenericNames, CSTRegularStructFields, CSTTupleStructFields},
    extension_traits::ParsableAstNode,
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_struct_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    let marker = parser.mark();
    parser.bump_str(SyntaxKind::StructKeyword, "struct");
    parser.expect_inline_whitespace();

    if !parser.try_bump_identifier_kind(SyntaxKind::TypeName) {
        state.restore(parser);

        return false;
    }

    parser.skip_whitespace();

    if CSTGenericNames::try_parse(parser) {
        parser.skip_whitespace();
    }

    match parser.peek_char() {
        Some('{') => {
            marker.start_node(parser, SyntaxKind::RegularStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = CSTRegularStructFields::try_parse(parser);

            parser.skip_whitespace();

            parser.expect_char('}');
        }
        Some('(') => {
            marker.start_node(parser, SyntaxKind::TupleStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = CSTTupleStructFields::try_parse(parser);

            parser.skip_whitespace();

            parser.expect_char(')');

            parser.skip_whitespace();

            parser.expect_char(';');
        }
        _ => {
            marker.start_node(parser, SyntaxKind::RegularStructDeclarationItem);

            parser.error("Expected '{' or '('");
        }
    }

    parser.finish_node();

    true
}

pub fn expect_struct_declaration_item_kind(parser: &mut Parser) {
    let marker = parser.mark();
    parser.bump_str(SyntaxKind::StructKeyword, "struct");
    parser.expect_inline_whitespace();

    parser.expect_identifier_kind(SyntaxKind::TypeName, "Expected struct name");

    parser.skip_whitespace();

    if CSTGenericNames::try_parse(parser) {
        parser.skip_whitespace();
    }

    match parser.peek_char() {
        Some('{') => {
            marker.start_node(parser, SyntaxKind::RegularStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = CSTRegularStructFields::try_parse(parser);

            parser.skip_whitespace();

            parser.expect_char('}');
        }
        Some('(') => {
            marker.start_node(parser, SyntaxKind::TupleStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = CSTTupleStructFields::try_parse(parser);

            parser.skip_whitespace();

            parser.expect_char(')');

            parser.skip_whitespace();

            parser.expect_char(';');
        }
        _ => {
            marker.start_node(parser, SyntaxKind::RegularStructDeclarationItem);

            parser.error("Expected '{' or '('");
        }
    }

    parser.finish_node();
}
