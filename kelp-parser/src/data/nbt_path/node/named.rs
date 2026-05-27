use crate::{
    cst::{CSTCompoundExpression, CSTNamedNBTPathNode},
    extension_traits::ParsableAstNode,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTNamedNBTPathNode {
    fn try_parse(parser: &mut Parser) -> bool {
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

        let _ = CSTCompoundExpression::try_parse(parser);

        parser.finish_node();

        true
    }
}
