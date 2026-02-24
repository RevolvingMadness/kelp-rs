use kelp_core::span::Span;

use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTNameEntitySelector, SyntaxKind::EntitySelectorName);

impl<'a> CSTNameEntitySelector<'a> {
    pub fn name_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorName {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorName {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(name_span) = self.name_span() {
            tokens.push(SemanticToken::new(name_span, SemanticTokenType::Variable));
        }
    }
}
