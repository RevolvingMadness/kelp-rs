use kelp_core::data_type::high::DataType;

use crate::{
    cst::CSTStructDeclarationItemField,
    data_type::{generics::try_parse_generic_names, lower_data_type, try_parse_data_type},
    parser::Parser,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_declaration_item_field(
    node: CSTStructDeclarationItemField,
) -> Option<(String, DataType)> {
    let name_token = node.name()?;
    let name = name_token.text().to_owned();
    let data_type = lower_data_type(node.data_type()?)?;

    Some((name, data_type))
}

pub fn bump_until_next_field_or_end(parser: &mut Parser) {
    let chars = parser.source[parser.pos..].chars();
    let mut length = 0;

    for char in chars {
        if char == ',' || char == '}' || char.is_alphabetic() {
            break;
        }

        length += char.len_utf8();
    }

    if length > 0 {
        parser.add_token(SyntaxKind::Garbage, length);
    }

    parser.try_bump_char(',');
}

#[must_use]
pub fn try_parse_struct_declaration_item(parser: &mut Parser) -> bool {
    let beginning = parser.save_state();

    parser.start_node(SyntaxKind::StructDeclarationItem);
    parser.bump_str(SyntaxKind::StructKeyword, "struct");
    parser.expect_inline_whitespace();

    if !parser.expect_identifier_kind(SyntaxKind::StructName, "Expected struct name") {
        parser.restore_state(beginning);

        return false;
    }

    parser.skip_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_whitespace();
    }

    if !parser.expect_char('{', "Expected '{'") {
        bump_until_next_field_or_end(parser);
    }

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        parser.start_node(SyntaxKind::StructDeclarationItemField);

        if !parser.expect_identifier_kind(SyntaxKind::StructFieldName, "Expected struct field name")
        {
            bump_until_next_field_or_end(parser);

            parser.finish_node();

            continue;
        }

        parser.skip_whitespace();

        let parsed_colon = parser.expect_char(':', "Expected ':'");

        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            if parsed_colon {
                parser.error("Expected data type");
            }

            bump_until_next_field_or_end(parser);

            parser.finish_node();

            continue;
        }

        parser.finish_node();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') && parser.peek_char() != Some('}') {
            parser.error("Expected ',' or '}'");
        }
    }

    parser.expect_char('}', "Expected '}'");

    parser.finish_node();

    true
}
