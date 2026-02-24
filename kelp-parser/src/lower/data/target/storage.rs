use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::resource_location::CSTResourceLocation,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTStorageDataTarget, SyntaxKind::StorageDataTarget);

impl<'a> CSTStorageDataTarget<'a> {
    #[must_use]
    pub fn storage_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn resource_location(&self) -> Option<CSTResourceLocation<'a>> {
        self.children().find_map(CSTResourceLocation::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(storage_keyword_span) = self.storage_keyword_span() {
            tokens.push(SemanticToken::new(
                storage_keyword_span,
                SemanticTokenType::Keyword,
            ));
        }

        if let Some(resource_location) = self.resource_location() {
            resource_location.collect_semantic_tokens(tokens);
        }
    }
}
