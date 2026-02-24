use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::entity_selector::CSTEntitySelector,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTEntityDataTarget, SyntaxKind::EntityDataTarget);

impl<'a> CSTEntityDataTarget<'a> {
    pub fn entity_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(entity_keyword_span) = self.entity_keyword_span() {
            tokens.push(SemanticToken::new(
                entity_keyword_span,
                SemanticTokenType::Keyword,
            ));
        }

        if let Some(selector) = self.selector() {
            selector.collect_semantic_tokens(tokens);
        }
    }
}
