use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::coordinates::CSTCoordinates,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTBlockDataTarget, SyntaxKind::BlockDataTarget);

impl<'a> CSTBlockDataTarget<'a> {
    pub fn block_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn coordinates(&self) -> Option<CSTCoordinates<'a>> {
        self.children().find_map(CSTCoordinates::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(block_keyword_span) = self.block_keyword_span() {
            tokens.push(SemanticToken::new(
                block_keyword_span,
                SemanticTokenType::Keyword,
            ));
        }

        if let Some(coordinates) = self.coordinates() {
            coordinates.collect_semantic_tokens(tokens);
        }
    }
}
