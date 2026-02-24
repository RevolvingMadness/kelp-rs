use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::expression::compound::CSTCompoundExpression,
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTNBTPathNamedNode, SyntaxKind::NBTPathNamed);

impl<'a> CSTNBTPathNamedNode<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('"') && parser.peek_identifier().is_none() {
            return false;
        }

        parser.start_node(SyntaxKind::NBTPathNamed);
        parser.try_parse_string_or_identifier();

        CSTCompoundExpression::try_parse(parser);

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn name<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text(text)))
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn compound(&self) -> Option<CSTCompoundExpression<'a>> {
        self.children().find_map(CSTCompoundExpression::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(token) = self
            .0
            .children_tokens()
            .find(|t| t.kind == SyntaxKind::Identifier || t.kind == SyntaxKind::String)
        {
            let token_type = if token.kind == SyntaxKind::String {
                SemanticTokenType::String
            } else {
                SemanticTokenType::Variable
            };
            tokens.push(SemanticToken::new(token.span, token_type));
        }

        if let Some(compound) = self.compound() {
            compound.collect_semantic_tokens(tokens);
        }
    }
}
