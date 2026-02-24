use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTCharExpression, SyntaxKind::CharExpression);

impl<'a> CSTCharExpression<'a> {
    pub fn value(&self, text: &str) -> Option<char> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Char {
                token.text(text).trim_matches('\'').chars().next()
            } else {
                None
            }
        })
    }
}
