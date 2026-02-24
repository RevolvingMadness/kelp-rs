use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTBindingPattern, SyntaxKind::BindingPattern);

impl CSTBindingPattern<'_> {
    #[must_use]
    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        tokens.push(SemanticToken::new(self.span(), SemanticTokenType::Variable));
    }
}
