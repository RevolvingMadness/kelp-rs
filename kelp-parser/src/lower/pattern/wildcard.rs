use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTWildcardPattern, SyntaxKind::WildcardPattern);

impl CSTWildcardPattern<'_> {
    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        tokens.push(SemanticToken::new(self.span(), SemanticTokenType::Variable));
    }
}
