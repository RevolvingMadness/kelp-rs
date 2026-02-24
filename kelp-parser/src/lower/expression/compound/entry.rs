use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::expression::CSTExpression,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(
    CSTCompoundExpressionEntry,
    SyntaxKind::CompoundExpressionEntry
);

impl<'a> CSTCompoundExpressionEntry<'a> {
    pub fn key_span(&self) -> Option<(SyntaxKind, Span)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier || token.kind == SyntaxKind::String {
                Some(token.as_tuple())
            } else {
                None
            }
        })
    }

    pub fn key<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier || token.kind == SyntaxKind::String {
                if token.kind == SyntaxKind::String {
                    Some((token.span, token.text(text).trim_matches('"')))
                } else {
                    Some((token.span, token.text(text)))
                }
            } else {
                None
            }
        })
    }

    pub fn value(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some((key_kind, key_span)) = self.key_span() {
            let token_type = if key_kind == SyntaxKind::String {
                SemanticTokenType::String
            } else {
                SemanticTokenType::Variable
            };

            tokens.push(SemanticToken::new(key_span, token_type));
        }

        if let Some(value) = self.value() {
            value.collect_semantic_tokens(tokens);
        }
    }
}
