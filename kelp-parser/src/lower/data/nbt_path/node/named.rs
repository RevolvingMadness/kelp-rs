use kelp_core::span::Span;

use crate::{
    cst_node, lower::expression::compound::CSTCompoundExpression, parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTNBTPathNamedNode, SyntaxKind::NBTPathNamed);

impl<'a> CSTNBTPathNamedNode<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('"') && parser.peek_identifier().is_none() {
            return false;
        }

        parser.start_node(SyntaxKind::NBTPathNamed);
        parser.try_parse_string_or_identifier();

        CSTCompoundExpression::try_parse(parser);

        parser.finish_node();

        true
    }

    pub fn name(&self) -> Option<(Span, &'a str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text))
            } else {
                None
            }
        })
    }

    pub fn compound(&self) -> Option<CSTCompoundExpression<'a>> {
        self.0.children().find_map(CSTCompoundExpression::cast)
    }
}
