use crate::{
    cst_node, lower::pattern::CSTPattern, semantic_token::SemanticToken, syntax::SyntaxKind,
};

cst_node!(CSTTuplePattern, SyntaxKind::TuplePattern);

impl<'a> CSTTuplePattern<'a> {
    pub fn patterns(&self) -> impl Iterator<Item = CSTPattern<'a>> {
        self.0.children().filter_map(CSTPattern::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        for pattern in self.patterns() {
            pattern.collect_semantic_tokens(tokens);
        }
    }
}
