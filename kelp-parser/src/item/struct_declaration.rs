use crate::{
    data_type::generics::try_parse_generic_names,
    parser::Parser,
    r#struct::{try_parse_struct_fields, try_parse_tuple_fields},
    syntax::SyntaxKind,
};

#[must_use]
pub fn try_parse_struct_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    let checkpoint = parser.checkpoint();
    parser.bump_str(SyntaxKind::StructKeyword, "struct");
    parser.expect_inline_whitespace();

    if !parser.try_bump_identifier_kind(SyntaxKind::TypeName) {
        parser.restore_state(state);

        return false;
    }

    parser.skip_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_whitespace();
    }

    match parser.peek_char() {
        Some('{') => {
            parser.start_node_at(checkpoint, SyntaxKind::StructStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = try_parse_struct_fields(parser);

            parser.skip_whitespace();

            parser.expect_char('}', "Expected '}'");
        }
        Some('(') => {
            parser.start_node_at(checkpoint, SyntaxKind::TupleStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = try_parse_tuple_fields(parser);

            parser.skip_whitespace();

            parser.expect_char(')', "Expected ')'");
        }
        _ => {
            parser.start_node_at(checkpoint, SyntaxKind::StructStructDeclarationItem);

            parser.error("Expected '{' or '('");
        }
    }

    parser.finish_node();

    true
}

#[must_use]
pub fn expect_struct_declaration_item_kind(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.bump_str(SyntaxKind::StructKeyword, "struct");
    parser.expect_inline_whitespace();

    parser.expect_identifier_kind(SyntaxKind::TypeName, "Expected struct name");

    parser.skip_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_whitespace();
    }

    match parser.peek_char() {
        Some('{') => {
            parser.start_node_at(checkpoint, SyntaxKind::StructStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = try_parse_struct_fields(parser);

            parser.skip_whitespace();

            parser.expect_char('}', "Expected '}'");
        }
        Some('(') => {
            parser.start_node_at(checkpoint, SyntaxKind::TupleStructDeclarationItem);

            parser.bump_char();

            parser.skip_whitespace();

            let _ = try_parse_tuple_fields(parser);

            parser.skip_whitespace();

            parser.expect_char(')', "Expected ')'");
        }
        _ => {
            parser.start_node_at(checkpoint, SyntaxKind::StructStructDeclarationItem);

            parser.error("Expected '{' or '('");
        }
    }

    parser.finish_node();

    true
}
