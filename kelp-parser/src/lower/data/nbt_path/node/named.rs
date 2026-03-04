use crate::{
    lower::expression::compound::try_parse_compound_expression, parser::Parser, syntax::SyntaxKind,
};

pub fn try_parse_named_nbt_path_node(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('"') && parser.peek_identifier().is_none() {
        return false;
    }

    parser.start_node(SyntaxKind::NamedNBTPathNode);
    if parser.peek_char() == Some('"') {
        let text = parser.peek_quoted_string().unwrap();
        parser.add_token(SyntaxKind::StringLiteral, text.len());
    } else if let Some(ident) = parser.peek_identifier() {
        parser.bump_identifier_kind(SyntaxKind::NamedNBTPathNodeName, ident);
    }

    let _ = try_parse_compound_expression(parser);

    parser.finish_node();

    true
}
